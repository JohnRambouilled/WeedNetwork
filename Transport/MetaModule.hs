{-# LANGUAGE TemplateHaskell,MultiParamTypeClasses #-}
module Transport.MetaModule where

import Transport.Control
import Transport.Sender
import Transport.Receiver

import Protocol
import Packet
import Class
import Timer
import Sources


import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Control.Monad
import Control.Monad.State hiding (get,put)
import Control.Lens

data TransportPacket = TransportSeg TrSegment | TransportControl TrControlMessage

fragSize = 1000 --- TODO

data Sender = Sender {_sToSend :: [SendBuf],
                      _sKill :: IO (),
                      _sCurrentDatagram :: DatagramID}

makeLenses ''Sender

senderSendBuf :: (MonadIO m) => MVar Timer -> SendBuf -> WriteFun -> IO () -> StateT Sender m [TrSegment]
senderSendBuf timerV buf wF break = let ret = buildSegments (sbDataID buf) $ zip [0..] $ sbBuf buf
                                    in do killRepeat <- liftIO $ repeatNTimes timerV (void $ spamPsh (last ret)) break  pshFreq pshRepeatParam 
                                          bufs <- use sToSend
                                          sKill .= killRepeat
                                          return ret
        where pshFreq = 5
              pshRepeatParam = 3
              spamPsh payload = runWriteFun wF $ encode $ TransportSeg payload

fragData :: RawData -> [RawData]
fragData raw = fst $ until (B.null . snd) f ([],raw)
  where f (frags,rest) = let (nfrag,nrest) = B.splitAt fragSize rest
                         in (frags ++ [nfrag],nrest)

senderQueueDatagram :: (MonadIO m) => MVar Timer ->  WriteFun -> IO () -> RawData -> StateT Sender m Bool
senderQueueDatagram timerV wF break raw  = do (toSend,curID) <- (,) <$> use sToSend <*> use sCurrentDatagram
                                              sCurrentDatagram += 1
                                              let sendBuf = SendBuf (curID + 1) raws
                                              sToSend .= toSend ++ [sendBuf]
                                              if null toSend 
                                                then and `fmap` (senderSendBuf timerV sendBuf wF break >>= mapM (liftIO . runWriteFun wF . encode))
                                                else return True
    where raws = fragData raw

onControlPacket :: (MonadIO m) => MVar Timer -> WriteFun -> IO () -> TrControlMessage -> StateT Sender m [TrSegment]
onControlPacket timerV writeFun break pkt = do buffers <-  use sToSend
                                               if null buffers then pure []
                                                  else let cur = head buffers
                                                       in do (retM,_) <- runStateT (runSendBuf pkt) cur
                                                             case retM of
                                                               Just pkts -> pure pkts
                                                               Nothing -> let rest = tail buffers
                                                                          in do sToSend .= rest
                                                                                use sKill >>= liftIO
                                                                                if null rest
                                                                                  then pure []
                                                                                  else do senderSendBuf timerV (head buffers) writeFun break
                                                                                  
--weedLink :: (WriteFun, BreakFun) -> IO (Callback, BrkClbck)
--weedLink (wF, bF) = 



openTCPCommunication :: (BreakFun -> IO (Callback, BrkClbck))
                       -> MVar Timer -> DiffTime -> DiffTime
                       -> SourceEntry -> ProtoRequest
                       -> IO (WriteFun, BreakFun)
openTCPCommunication clbkGen timerV tO ref sE pR = do 
                            sndV <- newMVar $ Sender [] (pure ()) 0
                            recvV <- newMVar $ RecvBuf [] 
                            let breakFun bF = BreakFun $ \d -> do withMVar sndV _sKill 
                                                                  runBreakFun bF $ d
                                writeFun (wF, bF) = WriteFun $ \d -> (runStateMVar sndV $ senderQueueDatagram timerV wF (void $ (runBreakFun $ breakFun bF) $ B.empty) d)
                            let clbkGen' wbF = do clbkT <- clbkGen $ breakFun $ snd wbF
                                                  pure (Callback $ weedCallback timerV sndV recvV clbkT wbF, snd clbkT) -- TOUT DOUX : kill l'entrée
                            (wF, bF) <- openCommunication clbkGen' timerV tO ref sE pR
                            pure (writeFun (wF, bF), breakFun bF)

weedCallback :: MVar Timer -> MVar Sender -> MVar RecvBuf -> (Callback,BrkClbck) -> (WriteFun, BreakFun) -> RawData -> IO ()
weedCallback timerV sndV rcvV (clbk,break) (wF, bF) rawData = case decodeMaybe rawData of
                                                                    Just (TransportSeg trSeg) -> modifyMVar_ rcvV $ onSeg trSeg
                                                                    Just (TransportControl trCtl) -> do seg <- runStateMVar sndV (onControlPacket timerV wF onTimeOut trCtl)
                                                                                                        forM_ seg $ runWriteFun wF . encode
     where onSeg trSeg (RecvBuf trList) = case runRecvBuf trList trSeg of
                                                Left (trList', (Just cm)) -> do runWriteFun wF . encode $ TransportControl cm
                                                                                pure $ RecvBuf trList'
                                                Left (trList', _) -> pure $ RecvBuf trList'                              
                                                Right (trList', cm) -> do runWriteFun wF . encode $ TransportControl cm
                                                                          runCallback clbk . B.concat $ map trData trList' 
                                                                          pure $ RecvBuf []
           onTimeOut = void $ runBrkClbck break B.empty >> runBreakFun bF B.empty
                   










instance Binary TransportPacket where
        put (TransportSeg x) = putWord8 0 >> put x
        put (TransportControl x) = putWord8 1 >> put x
        get = do x <- getWord8 
                 case x of
                   0 -> TransportSeg <$> get
                   1 -> TransportControl <$> get
                   _ -> fail "invalid transport packet"
