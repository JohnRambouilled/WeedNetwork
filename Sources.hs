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

-- | Arguments are : KeySize, User PrivKey (to sign as neighbor), Destinary PubKey, Road and Content
genNewPipeRequest :: Int -> PrivKey -> PubKey -> Road -> RawData -> IO (PrivKey, Packet)
genNewPipeRequest kSize uK destK r d = do (pub, priv) <- generateKeyPair kSize
                                          let l = length r
                                              kH = KeyHash $ pubKeyToHash pub
                                              epk = encode (kH, priv, destK)
                                              req = Request 0 (sign uK $ epk) l r epk d
                                          pure (priv, Introduce kH pub  (sign priv $ reqSourceHash kH req) $ IntroContent $ encode req)
    


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
pipesRoutingCallback ::  PrivKey -> SendFunction -> MVar Sources -> RoutingCB
pipesRoutingCallback uK send sV = genCallback sV inFun outFun
    where inFun req = return [req]
          outFun r (SourceAnswer sID sAnsM) = case sAnsM of
                                                Nothing -> pure []
                                                Just (dCB, pV) -> do (onBrk, pL) <- addNewPipe uK send pV r
                                                                     return [RoutingAnswer (onBrk >> chkSource pV sID) (Just [dCB]) pL ]
          chkSource pV sID = do b <- withMVar pV $ pure . M.null
                                if b then removeSourceEntry sV sID (encode ()) 
                                     else pure ()


addNewPipe :: MonadIO m => PrivKey -> SendFunction -> MVar Pipes -> Request -> m (IO (), [Request] )
addNewPipe uK send pV r = if roadPosition r == 0 then case decodeMaybe (encryptedPrivKey r) of
                                                        Nothing -> pure (pure (), [])
                                                        Just (kH, pK, destK) -> do epk <- liftIO $ encrypt destK $ encode ((kH, pK) :: (KeyHash, PrivKey))
                                                                                   let r' = r{encryptedPrivKey = epk}
                                                                                   onTO <- insertPipe (kH, pK) True r'{neighborSignature = sign uK $ reqNeighHash kH r'}
                                                                                   pure (onTO, [r'])
                                                 else case decodeMaybe . (decrypt uK) $ encryptedPrivKey r of
                                                        Nothing -> pure (pure (), [])
                                                        Just k -> insertPipe k False r >>= \onTO -> pure (onTO, [])
    where insertPipe :: MonadIO m => (KeyHash, PrivKey) -> Bool -> Request -> m (IO ())
          insertPipe k b r = liftIO $ modifyMVar pV $ insertPipeEntry k b r
          insertPipeEntry (kH, pK) b r pM =  do keepLog SourcesLog Important $ "Inserting pipe entry for pipe : " ++ (show . roadToRoadID $ road r) ++ "on Road : " ++ (show $ road r)
                                                let n = if b then (roadPosition r + 1) else (roadPosition r -1)
                                                pure (M.insert kH (PipesEntry (roadToRoadID $ road r)
                                                                           (genWriteFunction b kH pK n)
                                                                           (genBreakFunction b kH pK n) ) pM, onTimeOut kH)
          genWriteFunction :: Bool -> KeyHash -> PrivKey -> Number -> ComMessage -> IO Bool
          genWriteFunction b kH pK n cm = send $ DataPacket kH (sign pK $ pipeMessageHash kH b (encode cm)) $ DataContent . encode $ PipeData n b $ encode cm
          genBreakFunction :: Bool -> KeyHash -> PrivKey -> Number -> RawData -> IO Bool
          genBreakFunction b kH pK n m = send $ DataPacket kH (sign pK $ pipeMessageHash kH b m) $ DataContent . encode $ PipeExit n b m
          onTimeOut kH = modifyMVar_ pV (pure . M.delete kH)


