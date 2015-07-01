{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies #-}
module Sources where

import Control.Monad.State
import Control.Concurrent
import Data.List
import Data.Maybe
import Data.Binary

import Class
import Crypto
import qualified Data.Map as M
import Packet
import Routing
import Communication
import Log

type Sources = MapModule SourceEntry SourceID Request SourceAnswer
type SourceT = StateT Sources

type SourceCB = MapBehaviour SourceEntry SourceID Request SourceAnswer
data SourceAnswer = SourceAnswer { sourceAnsID :: SourceID,
                                   sourceAnswer :: Maybe (DataCB, MVar Pipes)} 

{- TODO Associer un pipe à une route -}
data SourceEntry =  SourceEntry {sourceID :: SourceID,
                                 sourcePipes :: MVar Pipes, 
                                 sourceFreeComID :: MVar [ComID],
                                 sourceCommunication :: MVar Communication}
instance Eq SourceEntry where sE == sE' = sourceID sE == sourceID sE'

instance MapModules SourceEntry SourceID Request SourceAnswer where
        packetKey (Request _ _ _ [] _ _ ) = return Nothing
        packetKey req = return . Just  $ if 0 == roadPosition req then last $ road req
                                                                 else head $ road req
        entryBehaviour (SourceEntry sID pV _ cV) = [\_ r -> return [SourceAnswer sID $ Just (genCommunicationCallback pV cV $ roadPosition r, pV)]]



genNewPipeRequest :: PrivKey -> PubKey -> DHPubKey -> Road -> RawData -> IO (PrivKey, PubKey, Packet)
genNewPipeRequest uK pK destK r d = do dhP <- generateDHPrivKey
                                       let (pP, (priv, pub)) = fromJust $ transmitKey destK dhP
                                           l = length r
                                           kH = KeyHash $ pubKeyToHash pub
                                           epk = encode (kH, (pP, (priv, pub)))
                                           req = Request 0 (sign uK pK $ epk) l r epk d
                                       pure (priv, pub, Introduce kH pub (sign priv pub $ reqSourceHash kH req) $ IntroContent $ encode req)



getSourceEntry :: MVar Sources -> SourceID -> IO (Maybe SourceEntry)
getSourceEntry sV sID = withMVar sV $ pure . M.lookup sID . keyMap

removeSourceEntry :: MVar Sources -> SourceID -> RawData -> IO () 
removeSourceEntry sV sID d = modifyMVar_ sV $ \s -> do let (msE, kM) = M.updateLookupWithKey (pure $ pure Nothing) sID $ keyMap s
                                                       maybe (pure ()) (breakComAndPipes) msE
                                                       pure s{keyMap = kM}
           where breakComAndPipes (SourceEntry _ pV iV cV) =  modifyMVar pV breakPipes >>= breakCom cV >> modifyMVar_ iV (pure $ pure [])
                 breakPipes pM = do forM pM $ ($d) . breakFun
                                    pure (M.empty, listToMaybe $ M.elems pM)
                 breakCom :: MVar Communication -> Maybe PipeEntry -> IO ()
                 breakCom cV pM = do modifyMVar_ cV $ \s -> do liftIO $ execStateT (mapM (sendBrk pM)  $ M.assocs (keyMap s)) s
                                                               pure $ newMapModule []
                 sendBrk pM (cID, ComEntry cCBL) = do sequence $ map ($ComExit cID (encode "source removed")) cCBL
                                                      case pM of
                                                        Nothing -> pure ()
                                                        Just p -> void . liftIO $ writeFun p $ ComExit cID (encode "source removed")
                                                     
                                                       



-- | Standard DestCallback for routing
pipesRoutingCallback ::  PrivKey -> PubKey -> SendFunction -> MVar Sources -> RoutingCB
pipesRoutingCallback uK pK send sV = genCallback sV inFun outFun
    where inFun req = return [req]
          outFun r (SourceAnswer sID sAnsM) = case sAnsM of
                                                Nothing -> pure []
                                                Just (dCB, pV) -> do (onBrk, pL) <- addNewPipe uK pK send pV r
                                                                     return [RoutingAnswer (onBrk >> chkSource pV sID) (Just [dCB]) pL ]
          chkSource pV sID = do b <- withMVar pV $ pure . M.null
                                if b then removeSourceEntry sV sID (encode ()) 
                                     else pure ()


addNewPipe :: MonadIO m => PrivKey -> PubKey -> SendFunction -> MVar Pipes -> Request -> m (IO (), [Request] )
addNewPipe uK pK send pV r = if roadPosition r == 0 then case decodeMaybe (encryptedPrivKey r) of
                                                        Nothing -> pure (pure (), [])
                                                        Just (kH, (pP, (prK, pK))) -> let r' = r{encryptedPrivKey = encode (pP :: DHPubKey)} in
                                                                         do onTO <- insertPipe (kH, prK, pK) True r'{neighborSignature = sign uK pK $ reqNeighHash kH r'}
                                                                            pure (onTO, [r'])
                                                 else case decodeMaybe $ encryptedPrivKey r of
                                                        Nothing -> pure (pure (), [])
                                                        Just pP -> do 
                                                                      let (prK, pK) = fromJust $ (exctractKey pP =<< privKeyToDHPrivKey uK)
                                                                          kH = KeyHash $ pubKeyToHash pK
                                                                      insertPipe (kH, prK, pK) False r >>= \onTO -> pure (onTO, [])
    where insertPipe :: MonadIO m => (KeyHash, PrivKey, PubKey) -> Bool -> Request -> m (IO ())
          insertPipe k b r = liftIO $ modifyMVar pV $ insertPipeEntry k b r
          insertPipeEntry (kH, prK, pK) b r pM = do keepLog SourcesLog Important $ "Inserting pipe entry for pipe : " ++ (show . roadToRoadID $ road r) ++ "on Road : " ++ (show $ road r)
                                                    let n = if b then (roadPosition r + 1) else (roadPosition r -1)
                                                    pure (M.insert kH (PipesEntry (roadToRoadID $ road r)
                                                                           (genWriteFunction b kH prK pK n)
                                                                           (genBreakFunction b kH prK pK n) ) pM, onTimeOut kH)
          genWriteFunction :: Bool -> KeyHash -> PrivKey -> PubKey -> Number -> ComMessage -> IO Bool
          genWriteFunction b kH prK pK n cm = send $ DataPacket kH (sign prK pK $ pipeMessageHash kH b (encode cm)) $ DataContent . encode $ PipeData n b $ encode cm
          genBreakFunction :: Bool -> KeyHash -> PrivKey -> PubKey -> Number -> RawData -> IO Bool
          genBreakFunction b kH prK pK n m = send $ DataPacket kH (sign prK pK $ pipeMessageHash kH b m) $ DataContent . encode $ PipeExit n b m
          onTimeOut kH = modifyMVar_ pV (pure . M.delete kH)


