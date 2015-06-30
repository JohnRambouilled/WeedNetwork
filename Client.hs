{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ExistentialQuantification #-}
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

import Class
import Communication
import Crypto
import Packet
import Routing
import Sources
import Ressource
import Neighborhood
import Protocol
import Timer
import Log

data Identity = Identity {clientSourceID :: SourceID,
                          privateKey :: PrivKey,
                          publicKey :: PubKey}

data Client = Client {cidentity :: Identity,
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

genClient :: SourceID -> PrivKey -> PubKey -> (Packet -> IO Bool) -> IO Client
genClient uID privK pubK send = do  timerV <- newMVar $ Timer M.empty [1..]
                                    protoV <- newMapMVar [] 
                                    sourceV <- newMapMVar [pipesNewSourceCallback timerV seedComTimeOut protoV] 
                                    routV <- newMVar $ Routing routingRelayCallback $ pipesRoutingCallback privK send sourceV
                                    resV <- newEmptyMVar 
                                    putMVar resV $ newMapModule [newDefaultBehaviour resV timerV  uID]
                                    neighV <- newMVar $ Neighborhood [registerRessourceModule uID privK resV]
                                    cryptoV <- newEmptyMVar
                                    putMVar cryptoV $ newMapModule [neighCryptoCallback cryptoV timerV neighV, routingCryptoCallback cryptoV timerV pubK privK uID routV]
                                    let cID = Identity uID privK pubK
                                    pure $ Client cID send timerV cryptoV neighV resV routV sourceV protoV
        where newMapMVar = newMVar . newMapModule

insertProtoCallback :: Client -> ProtoID -> ProtoCallback -> IO ()
insertProtoCallback c rID pCB = do keepLog ClientLog Important "insertion d'un protoCallback"
                                   modifyMVar_ (cprotocol c) $ liftIO . (snd <$>) . runStateT (insertMapBehaviour rID $ ProtoEntry pCB)



newtype RoadChoice = RoadChoice {choseRoad :: Road -> SourceID -> RessourceCert -> RawData -> IO Bool}

cleanSourceEntryList :: [SourceEntry] -> IO [SourceEntry]
cleanSourceEntryList = filterM $ (flip withMVar $ pure . M.null) . sourcePipes

connectToRessource :: Client -> TVar [SourceID] -> RoadChoice -> RessourceID -> IO ()
connectToRessource c sV rC rID = insertRessourceEntry (cressource c) rID $ resCallback c $ csources c
    where resCallback :: Client -> MVar Sources -> RessourceCB
          resCallback _ _ (Research _ _ _ _) = pure [] --TODO : se mettre a relayer les réponses
          resCallback c srcV a@(Answer cert _ r sID d) = liftIO $ let uID = clientSourceID $ cidentity c
                                                                      r' = uID : r in
                                                                  do keepLog ClientLog Normal $ "answer received on road : " ++ show r' ++ " calling roadChoice"
                                                                     (atomically . writeTVar sV) =<< (filterM  ((isJust <$>) . extractRoads (csources c))) =<< (atomically $ readTVar sV)
                                                                     b <- (choseRoad rC) r' sID cert d
                                                                     if b then do keepLog ClientLog Important $ "interesting road, opening pipe..."
                                                                                  forkIO $ openNewPipeIO c (cResSourceKey cert) r' d -- TODO :c'est vraiment utile de se passer de la data?
                                                                                  keepLog ClientLog Normal $ "Pipe opened, registering the source"
                                                                                  atomically $ (readTVar sV >>= (writeTVar sV) . nub . (sID :))
                                                                                  pure . maybeToList $ relayAnswerPacket uID a
                                                                          else pure . maybeToList $ relayAnswerPacket uID a
                                                                          

extractRoads :: MVar Sources -> SourceID -> IO (Maybe [RoadID])
extractRoads sourcesV sID = do pipes <- getSourceEntry sourcesV sID >>= maybe (pure Nothing) (pure . Just . sourcePipes)  
                               if isNothing pipes then return Nothing
                                                  else (Just . map roadID .M.elems) <$> readMVar (fromJust pipes)
--                              pipes <- (sourcePipes . fromJust) <$> getSourceEntry sourcesV sID   



openNewPipeIO :: Client -> PubKey -> Road -> RawData -> IO ()
openNewPipeIO c k r d = runStateT (openNewPipe k r d) c >> pure ()

openNewPipe :: MonadIO m => PubKey -> Road -> RawData -> ClientT m Bool 
openNewPipe dK r d = do (uK,uID) <- (,) <$> gets (privateKey . cidentity) <*> gets (clientSourceID . cidentity)
                        (cryptoM, timerM, send) <- (,,) <$> gets ccrypto <*> gets ctimer <*> gets csender
                        liftIO . keepLog ClientLog Normal $ "generating new request"
                        (pipePK, req) <- liftIO $ genNewPipeRequest uK dK r d
                        let sendReq = sendRequest send cryptoM req pipePK
                        liftIO $ do _ <- repeatEach timerM (void sendReq) pipeRefreshTO 
                                    sendReq


-- | Process a request (pass it to the cryptoModule), then send it on the network
sendRequest :: SendFunction -> MVar Crypto -> Packet -> PrivKey -> IO Bool
sendRequest send crypto p pK = do keepLog ClientLog Normal "Passing request to crypto"
                                  reqL <- runModule crypto p 
                                  keepLog ClientLog Normal "Sending request"
                                  gen <- liftIO genRnd
                                  head <$> mapM (signAndSend gen) reqL
   where signAndSend g p = case decodeMaybe (runIntroContent $ introContent p) :: Maybe Request of
                                        Nothing -> pure False
                                        Just req -> send $ p{sig = sign g pK $ reqSourceHash (keyID p) req}


genNeighHello :: CPRG g => g -> Identity -> RawData -> Packet
genNeighHello gen i d = Introduce kID (publicKey i) (sign gen (privateKey i) $ encode (kID, publicKey i, nHello)) $ IntroContent nHello
       where nHello = encode $ NeighHello d 
             kID = keyHash $ clientSourceID i

instance Modules Client Packet Packet where
        onPacket pkt = do cryptoM <- gets ccrypto
                          liftIO $ runModule cryptoM pkt 


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





