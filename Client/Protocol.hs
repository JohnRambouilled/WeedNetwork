{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies, FlexibleContexts #-}
module Client.Protocol where

import Control.Monad.State hiding (put, get)
import qualified Control.Monad.State  as S (get)
import Control.Concurrent
import Data.Binary
import qualified Data.Map as M
import Data.Maybe
import Data.List

import Client.Class
import Client.Crypto
import Client.Packet
import Client.Routing
import Client.Communication
import Client.Pipes
import Client.Sources
import Timer
import Log
import Control.Monad.Reader

newtype Callback = Callback {runCallback :: RawData -> IO ()}
newtype BrkClbck = BrkClbck {runBrkClbck :: RawData -> IO ()}
newtype WriteFun = WriteFun {runWriteFun :: RawData -> IO Bool}
newtype BreakFun = BreakFun {runBreakFun :: RawData -> IO Bool}


type Protocol = MapModule ProtoEntry ProtoID ProtoRequest ProtoCallback
newtype ProtoCallback = ProtoCallback  {runProtoCallback :: RawData -> (WriteFun, BreakFun) -> IO (Maybe (Callback, BrkClbck))}

data ProtoEntry = ProtoEntry {protoCallback :: ProtoCallback}

instance MapModules ProtoEntry ProtoID ProtoRequest ProtoCallback where
        packetKey = Just . protoID <$> ask
        entryBehaviour = pure . pure . pure . protoCallback
		    -- = pure . flip . pure . pure . pure . pure . protoCallback
                    -- = pure . pure . pure . pure . pure . protoCallback   <- [TODO] expliquer pourquoi ça marche aussi!
                --Heeeuuu, t'es sure? ghc a l'air content


leechTimeOut = 150 :: DiffTime
leechRefreshTime = 140 :: DiffTime

openCommunication :: ((WriteFun, BreakFun) -> IO (Callback, BrkClbck))
                    -> MVar Timer -> DiffTime -> DiffTime
                    -> SourceEntry -> ProtoRequest
                    -> IO (WriteFun, BreakFun)
openCommunication clbkGen tV timeOut refreshTime sE  = openCom clbkGen genRefrKill sE
  where genRefrKill cID = do (ref, kill) <- registerTimerM tV timeOut (closeComIO fV cV cID >> pure True)
                             killRep <- repeatEach tV (void $ sendToSource sE $ ComInit cID $ encode "PING") refreshTime
                             pure (ref, kill >> killRep)
        (fV, cV) = ((,) <$> sourceFreeComID <*> sourceCommunication) sE


openCommunicationTO :: ((WriteFun, BreakFun) -> IO (Callback, BrkClbck))
                    -> MVar Timer -> DiffTime
                    -> SourceEntry -> ProtoRequest
                    -> IO (WriteFun, BreakFun)
openCommunicationTO clbkGen tV timeOut sE = openCom clbkGen genRefrKill sE
  where genRefrKill cID = registerTimerM tV timeOut (closeComIO fV cV cID >> pure True)
        (fV, cV) = ((,) <$> sourceFreeComID <*> sourceCommunication) sE

openCom :: ((WriteFun, BreakFun) -> IO (Callback, BrkClbck))
                    -> (ComID -> IO (IO (), IO ()))
                    -> SourceEntry -> ProtoRequest
                    -> IO (WriteFun, BreakFun)
openCom clbkGen toKill sE@(SourceEntry sID _ fV cV) pR  = do
        keepLog ProtocolLog Important $ "OpenCommunication called for source : " ++ show sID
        cID <- modifyMVar fV $ pure . (\s -> (tail s, head s))
        (refresh, kill) <- toKill cID
        let wbF = genWriteBreakFun kill sE cID
            comI = ComInit cID $ encode pR
        keepLog ProtocolLog Normal $ "sending ComINIT : " ++ show comI
        keepLog ProtocolLog Normal . ("Comlist : " ++) =<< withMVar cV (pure . show)
        ret <- sendToSource sE comI
        if ret then do keepLog ProtocolLog Normal $ "generating Callbacks"
                       clbks <- liftIO $ clbkGen wbF
                       insertComCallback cV cID $ ask >>= \comMsg -> stdCallback refresh kill clbks comMsg
               else do keepLog ProtocolLog Error "fail to send comINIT"
                       kill
        pure wbF
      where stdCallback :: IO () -> IO () -> (Callback, BrkClbck) -> ComMessage -> ComCB
            stdCallback _ _ (clbk,_) (ComData _ d) = do void . liftIO . forkIO $ runCallback  clbk  $ d
                                                        pure []
            stdCallback _ kill (_, brk) (ComExit cID d) = do closeCom fV cID
                                                             liftIO $ kill
                                                             void . liftIO . forkIO $ runBrkClbck brk d
                                                             pure []
            stdCallback refresh _ (_,_) (ComInit cID d) = do keepLog ProtocolLog Error $ "ComInit on used comID (Leech) refreshing entry " ++ show cID
                                                             liftIO $ refresh
                                                             pure []





genProtoCallback :: MVar Timer -> DiffTime -> MVar Protocol -> SourceEntry -> ComCB
genProtoCallback tV timeOut pv sE@(SourceEntry  _ _ fV cV) = defaultComCallback pv inFun outFun
    where inFun = decodeMaybe . comContent :: ComMessage -> Maybe ProtoRequest
          outFun :: ComID -> ProtoRequest -> [ProtoCallback] -> IO (Maybe ComCB)
          outFun cID pR r = do (refresh, kill) <- registerTimerM tV timeOut (closeComIO fV cV cID >> pure True)
                               let (wF,bF) = genWriteBreakFun kill sE cID
                               cbkM <- liftIO $ (runProtoCallback $ head r) (protoContent pR) (wF,bF) --Warning head
                               case cbkM of
                                Nothing -> do runBreakFun bF $ encode "unable to open connection"
                                              return Nothing
                                Just (clbk, brk) -> do liftIO $ modifyMVar_ fV $ pure . delete cID
                                                       return . Just $ ask >>= comClbck refresh kill clbk brk
          comClbck refresh kill clbk brk cm = do
                       case cm of ComData _ d -> liftIO $ do keepLog ProtocolLog Normal $ "running callback on message : " ++ show cm
                                                             void . forkIO $ runCallback clbk $ d
                                  ComExit cID d -> do closeCom fV cID
                                                      liftIO $ kill
                                                      keepLog ProtocolLog Important $ "calling breakCallback on message : " ++ show cm
                                                      S.get >>= keepLog CommunicationLog Normal . show
                                                      liftIO . void . forkIO $ runBrkClbck brk $ d
                                  c@(ComInit cID _) -> do keepLog ProtocolLog Error $ "ComInit on used comID (Seed) refreshing entry " ++ show cID
                                                          liftIO $ sendToSource sE c
                                                          liftIO $ refresh
                       pure []

-- | Standard defaultCallback for pipes : creates a new communication module and return the corresponding callback
pipesNewSourceCallback :: MVar Timer -> DiffTime -> MVar Protocol -> SourceCB
pipesNewSourceCallback tV timeOut protoV = do
     (r,sIDM) <- (,) <$> ask <*> packetKey 
     case sIDM of
           Nothing -> return []
           Just sID -> do cV <- liftIO $ newMVar (newMapModule [])
                          pV <- liftIO $ newMVar (Pipes M.empty [])
                          cidV <- liftIO $ newMVar (map ComID [1..])
                          let sE = SourceEntry sID pV cidV cV
                          insertMapBehaviour sID sE
                          liftIO $ modifyMVar_ cV $ \s -> pure s{defaultBehaviour = [genProtoCallback tV timeOut protoV sE]}
                          return [SourceAnswer sID $ Just (genCommunicationCallback pV cV (roadPosition r) $ roadToRoadID (road r), pV)]





genWriteBreakFun :: IO () -> SourceEntry -> ComID -> (WriteFun, BreakFun)
genWriteBreakFun kill sE@(SourceEntry sID _ fV cV) cID  = (WriteFun wrFun, BreakFun brFun)
    where wrFun d = do keepLog CommunicationLog Normal $ "sending ComMessage to source : " ++ show sID ++ " on Com : " ++ show cID
                       sendToSource sE $ ComData cID d
          brFun d = do closeComIO fV cV cID
                       kill
                       keepLog CommunicationLog Important $ "[Communication] Sending ComExit  to source : " ++ show sID ++ " on Com : " ++ show cID
                       sendToSource sE $ ComExit cID d





data ProtoRequest = ProtoRequest {protoID ::ProtoID, protoContent :: RawData}
instance Binary ProtoRequest where put (ProtoRequest pID cnt) = put pID >> put cnt
                                   get = ProtoRequest <$> get <*> get
newtype ProtoID = ProtoID RawData deriving (Eq, Ord)
instance Binary ProtoID where put (ProtoID pID) = put pID
                              get = ProtoID <$> get



{-
genRestrictedWriteBreakFun :: SourceEntry -> ComID -> IO (Maybe (WriteFun, BreakFun))
genRestrictedWriteBreakFun sE@(SourceEntry sID _ fV cV) cID  = do pL <- withMVar (sourcePipes sE) (pure . M.elems)
                                                                  case pL of [] ->  pure Nothing
                                                                             p:_ -> let w = writeFun p in
                                                                                    pure $ Just (WriteFun $ wrFun w, BreakFun $ brFun w)
    where wrFun w d = do keepLog CommunicationLog Normal $ "sending ComMessage to source : " ++ show sID ++ " on Com : " ++ show cID
                         w $ ComData cID d
          brFun w d = do closeComIO fV cV cID
                         keepLog CommunicationLog Important $ "[Communication] Sending ComExit  to source : " ++ show sID ++ " on Com : " ++ show cID
                         w $ ComExit cID d

openRestrictedCommunication :: SourceEntry -> ProtoRequest -> ((WriteFun, BreakFun) -> IO (Callback, BrkClbck)) -> IO (Maybe (WriteFun, BreakFun))
openRestrictedCommunication sE pR = openCom sE pR $ genRestrictedWriteBreakFun sE

-}
