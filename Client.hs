{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, ExistentialQuantification #-}
module Client where

import Control.Monad.State
import Control.Concurrent
import qualified Data.Map as M
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.List
import Data.Maybe
import Data.Binary hiding (put, get)
import Crypto.Random
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS.Lazy

import Client.Class
import Client.Communication
import Client.Crypto
import Client.Packet
import Client.Routing
import Client.Sources
import Client.Ressource
import Client.Neighborhood
import Client.Protocol
import Client.Pipes
import Timer
import Log


data Identity = Identity {clientSourceID :: SourceID,
                          privateKey :: PrivKey,
                          publicKey :: PubKey}

data Client = Client {cidentity :: Identity,
                      cRandomGen :: MVar RandomGen,
                      csender :: Packet -> IO Bool,
                      ctimer :: MVar Timer,
        
                      ccrypto :: MVar Crypto,

                      cneighborhood :: MVar Neighborhood,
                      cressource :: MVar RessourceModule,
                
                      crouting :: MVar Routing,
                      csources :: MVar Sources,
                      cprotocol :: MVar Protocol}
type ClientT = StateT Client


offerRessourceID :: Client -> RessourceID -> RawData -> IO ()
offerRessourceID c rID d =  do tV <- newEmptyMVar
                               insertRessourceEntry (cressource c) rID $ genRessourceCallback tV uK pK sID rID d
         where (Identity sID pK uK) = cidentity c    


insertRessourceEntry :: MVar RessourceModule -> RessourceID -> RessourceCB -> IO ()
insertRessourceEntry rM rID rCB = runStateMVar rM $ insertMapBehaviourWith mergeRE rID . RessourceEntry $ pure  rCB
    where mergeRE (RessourceEntry l1) (RessourceEntry l2) = RessourceEntry (l1++l2)


seedComTimeOut = 200 :: DiffTime

genClient :: MVar RandomGen -> SourceID -> PrivKey -> PubKey -> (Packet -> IO Bool) -> IO Client
genClient gen uID privK pubK send = do  timerV <- newMVar $ Timer M.empty [1..]
                                        protoV <- newMapMVar [] 
                                        sourceV <- newMapMVar [pipesNewSourceCallback timerV seedComTimeOut protoV] 
                                        routV <- newMVar $ Routing routingRelayCallback $ pipesRoutingCallback privK pubK send sourceV
                                        resV <- newEmptyMVar 
                                        putMVar resV $ newMapModule [ressourceDefaultBehaviour resV timerV  uID]
                                        neighV <- newMVar $ Neighborhood [registerRessourceModule uID privK pubK resV]
                                        cryptoV <- newEmptyMVar
                                        putMVar cryptoV $ newMapModule [neighCryptoCallback cryptoV timerV neighV, routingCryptoCallback cryptoV timerV pubK privK uID routV]
                                        let cID = Identity uID privK pubK
                                        pure $ Client cID gen send timerV cryptoV neighV resV routV sourceV protoV
        where newMapMVar = newMVar . newMapModule

insertProtoCallback :: Client -> ProtoID -> ProtoCallback -> IO ()
insertProtoCallback c rID pCB = do keepLog ClientLog Important "insertion d'un protoCallback"
                                   modifyMVar_ (cprotocol c) $ liftIO . (snd <$>) . runStateT (insertMapBehaviour rID $ ProtoEntry pCB)



newtype RoadChoice = RoadChoice {choseRoad :: Road -> SourceID -> RessourceCert -> RawData -> IO Bool}

cleanSourceEntryList :: [SourceEntry] -> IO [SourceEntry]
cleanSourceEntryList = filterM $ (flip withMVar $ pure . M.null . pipesMap) . sourcePipes

connectToRessource :: Client -> TVar [SourceID] -> RoadChoice -> RessourceID -> IO ()
connectToRessource c sV rC rID = insertRessourceEntry (cressource c) rID $ resCallback c (csources c) =<< ask
    where resCallback :: Client -> MVar Sources -> RessourcePacket -> RessourceCB
          resCallback _ _ (Research _ _ _ _) = pure [] --TODO : se mettre a relayer les réponses
          resCallback c srcV a@(Answer cert _ r sID d) = liftIO $ let uID = clientSourceID $ cidentity c
                                                                      r' = uID : r in
                                                                  do keepLog ClientLog Normal $ "answer received on road : " ++ show r' ++ " calling roadChoice"
                                                                     (atomically . writeTVar sV) =<< (filterM  ((isJust <$>) . extractRoads (csources c))) =<< (atomically $ readTVar sV)
                                                                     b <- (choseRoad rC) r' sID cert d
                                                                     if b then do keepLog ClientLog Important $ "interesting road, opening pipe..."
                                                                                  forkIO $ openNewPipeIO c (cResSourceDHKey cert) r' d -- TODO :c'est vraiment utile de se passer de la data?
                                                                                  keepLog ClientLog Normal $ "Pipe opened, registering the source"
                                                                                  atomically $ (readTVar sV >>= (writeTVar sV) . nub . (sID :))
                                                                                  pure . maybeToList $ relayAnswerPacket uID a
                                                                          else pure . maybeToList $ relayAnswerPacket uID a
                                                                          

extractRoads :: MVar Sources -> SourceID -> IO (Maybe [RoadID])
extractRoads sourcesV sID = do pipes <- getSourceEntry sourcesV sID >>= maybe (pure Nothing) (pure . Just . sourcePipes)  
                               if isNothing pipes then return Nothing
                                                  --else (Just . map (roadID . roadSpecs) . M.elems) <$> readMVar (fromJust pipes)
                                                  else Just . pipesList <$> readMVar (fromJust pipes)
--                              pipes <- (sourcePipes . fromJust) <$> getSourceEntry sourcesV sID   



openNewPipeIO :: Client -> DHPubKey -> Road -> RawData -> IO ()
openNewPipeIO c k r d = runRWST (openNewPipe k r d) () c >> pure ()

openNewPipe :: (MonadState Client m, MonadWriter Log m, MonadIO m) => DHPubKey -> Road -> RawData -> m Bool 
openNewPipe dK r d = do (uK,uID, pK) <- (,,) <$> gets (privateKey . cidentity) <*> gets (clientSourceID . cidentity) <*> gets (publicKey . cidentity)
                        gV <- gets cRandomGen
                        (cryptoM, timerM, send) <- (,,) <$> gets ccrypto <*> gets ctimer <*> gets csender
                        liftIO . keepLog ClientLog Normal $ "generating new request"
                        (pipePrK, pipePK, req) <- liftIO $ genNewPipeRequest gV uK pK dK r d
                        let sendReq = sendRequest send cryptoM req pipePrK pipePK
                            sendReq' = runWriterT $ sendRequest send cryptoM req pipePrK pipePK
                        liftIO $ repeatEach timerM (void sendReq') pipeRefreshTO 
                        sendReq


-- | Process a request (pass it to the cryptoModule), then send it on the network
sendRequest :: (MonadWriter Log m, MonadIO m) => SendFunction -> MVar Crypto -> Packet -> PrivKey -> PubKey -> m Bool
sendRequest send crypto p prK pK = do keepLog ClientLog Normal "Passing request to crypto"
                                      (reqL, logs) <- liftIO $ runModule crypto p 
                                      tell logs
                                      keepLog ClientLog Normal "Sending request"
                                      head <$> (liftIO $ mapM signAndSend reqL)
   where signAndSend p = case decodeMaybe (runIntroContent $ introContent p) :: Maybe Request of
                                        Nothing -> pure False
                                        Just req -> send $ p{sig = sign prK pK $ reqSourceHash (keyID p) req}


genNeighHello :: Identity -> RawData -> Packet
genNeighHello i d = Introduce kID (publicKey i) (sign (privateKey i) (publicKey i) $ encode (kID, publicKey i, nHello)) $ IntroContent nHello
       where nHello = encode $ NeighHello d 
             kID = keyHash $ clientSourceID i

instance Modules Client Packet [Packet] where
        onPacket = do cryptoM <- gets ccrypto
                      (pL, logs) <- ask >>= (liftIO . runModule cryptoM)
                      tell logs
                      pure pL


dumpClient :: MonadIO m => ClientT m String
dumpClient = (concat <$>) . sequence $ [("CLIENT : " ++) . show <$> gets (clientSourceID . cidentity), pure "\n",
                                        pure "CryptoModule : ", showModule ccrypto "Crypto", pure "\n",
                                        pure "RessourceModule : ", showModule cressource "Ressource", pure "\n",
                                        pure "SourceModule : ", showModule csources "Sources", pure "\n",
                                        pure "Communications :", dumpComIDS, pure "\n \n"]
    where showModule acc msg = do 
                mV <- gets acc   
                liftIO . withMVar mV $ \c -> pure $ "module " ++ msg ++ " : " ++ show c


dumpComIDS :: MonadIO m => ClientT m String
dumpComIDS = do srcs <- (map sourceCommunication . M.elems . keyMap) <$> (gets csources >>= (liftIO . readMVar))
                concat <$> (forM srcs $ liftIO . (show . M.keys . keyMap <$>) . readMVar)





