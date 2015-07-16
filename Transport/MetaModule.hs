{-# LANGUAGE TemplateHaskell,MultiParamTypeClasses, FlexibleContexts #-}
module Transport.MetaModule where

import Transport.Control
import Transport.Sender
import Transport.Receiver

import Client.Protocol
import Client.Packet
import Client.Class
import Timer
import Log 
import Client.Sources

import Data.List
import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Control.Monad
import Control.Monad.State hiding (get,put)
import Control.Lens



onControlPacket :: (SW Sender m) => MVar Timer -> (WriteFun, BreakFun) -> IOLog () -> TrControlMessage -> m [TrSegment]
onControlPacket timerV wrBrkFun break pkt = do buffers <-  use sToSend
                                               keepL Normal $ "control packet received : " ++ show pkt
                                               if null buffers then keepL Error "No datagram in buffer" >> pure []
                                                  else let cur = head buffers
                                                       in do (retM,_) <- runStateT (runSendBuf pkt) cur
                                                             case retM of
                                                               Just pkts -> keepL Normal "sending correction packets" >> pure pkts
                                                               Nothing -> let rest = tail buffers
                                                                          in do sToSend .= rest
                                                                                keepL Important "Datagram successfully sent, flushing buffer"
                                                                                use sKill >>= liftLog
                                                                                if null rest
                                                                                  then keepL Normal "Datagram queue empty" >> pure []
                                                                                  else do keepL Normal "Sending the next datagram" >> senderSendBuf timerV (head rest) wrBrkFun break
                                                                                  


openTCPCommunication :: ((WriteFun, BreakFun) -> IOLog (Callback, BrkClbck))
                       -> MVar Timer -> DiffTime -> DiffTime
                       -> SourceEntry -> ProtoRequest
                       -> IOLog (WriteFun, BreakFun)
openTCPCommunication clbkGen timerV tO ref sE pR = do 
                            keepL Important $ "New TCP-communication opened to source : " ++ show (sourceID sE)
                            sndV <-  liftIO $ newMVar $ Sender [] (pure ()) 0
                            recvV <- liftIO $ newMVar $ RecvBuf [] 
                            let genWBF = genTCPWriteBreakFun timerV sndV
                                clbkGen' wbF = genTCPCallback timerV sndV recvV wbF <$> clbkGen (genWBF wbF)
                            genWBF <$> openCommunication clbkGen' timerV tO ref sE pR
                          


genTCPWriteBreakFun :: MVar Timer -> MVar Sender -> (WriteFun, BreakFun) -> (WriteFun, BreakFun)
genTCPWriteBreakFun timerV sndV wbF = (writeFun, breakFun)
        where breakFun = BreakFun $ \d -> do keepL Important "BreakFun called"
                                             runSWMVar sndV $ senderQueueSendBuf timerV wbF kill $ BufKill d
              writeFun = WriteFun $ \d -> runSWMVar sndV $ senderQueueDatagram timerV wbF kill d
              kill = do sndr <- _sKill =<< (liftIO $ readMVar sndV)
                        void . runBreakFun (snd wbF) $ B.empty

genTCPCallback :: MVar Timer -> MVar Sender -> MVar RecvBuf -> (WriteFun, BreakFun) -> (Callback, BrkClbck) -> (Callback, BrkClbck)
genTCPCallback timerV sndV rcvV wbF clbks = (Callback clbk, BrkClbck brck)
    where clbk = weedCallback timerV sndV rcvV clbks wbF 
          brck d = (liftIO $ readMVar sndV) >>= _sKill >> (runBrkClbck (snd clbks) $ d)


weedCallback :: MVar Timer -> MVar Sender -> MVar RecvBuf -> (Callback,BrkClbck) -> (WriteFun, BreakFun) -> RawData -> IOLog ()
weedCallback timerV sndV rcvV (clbk,break) (wF, bF) rawData = do keepLog TransportLog Normal "Callback called"
                                                                 case decodeMaybe rawData of
                                                                    Just (TransportSeg trSeg) -> modifySWMVar rcvV $ ((\a -> (a,())) <$>) . onSeg trSeg
                                                                    Just (TransportControl trCtl) -> do seg <- runSWMVar sndV (onControlPacket timerV (wF, bF) onTimeOut trCtl)
                                                                                                        forM_ seg $ sendTRSegment wF
                                                                    Nothing -> keepLog TransportLog Error "[WEEDCallback] fail to decode transport packet"
     where onSeg trSeg (RecvBuf trList) = do keepL Normal $ "received packet : "-- ++ show trSeg
                                             case runRecvBuf trList trSeg of
                                                Left (trList', (Just cm)) -> do runWriteFun wF . encode $ TransportControl cm
                                                                                keepLog TransportLog Normal $ "Datagram received from wrong datagramID "  ++ show trSeg
                                                                                keepL Normal $ "buffer : " ++ show trList'
                                                                                pure $ RecvBuf trList'
                                                Left (trList', _) -> do keepL Normal "Datagram added to buffer"
                                                                        pure $ RecvBuf trList'                              
                                                Right (trList', cm) -> do runWriteFun wF . encode $ TransportControl cm
                                                                          keepLog TransportLog Normal "running callback (WeedCallback, Metamodule)"
                                                                          let r = B.concat $ map trData trList' 
                                                                          runCallback clbk $ r
                                                                          pure $ RecvBuf []
           onTimeOut = void $ runBrkClbck break B.empty >> runBreakFun bF B.empty
                   



protoTCPCallback :: MVar Timer -> ProtoCallback -> ProtoCallback
protoTCPCallback timerV prtClbk = ProtoCallback protoClbk 
    where protoClbk :: RawData -> (WriteFun, BreakFun) -> IOLog (Maybe (Callback, BrkClbck))
          protoClbk d wbF0 = do 
                            sndV <- liftIO $ newMVar $ Sender [] (pure ()) 0
                            recvV <- liftIO $ newMVar $ RecvBuf [] 
                            let wbF = genTCPWriteBreakFun timerV sndV wbF0
                            (genTCPCallback timerV sndV recvV wbF0 <$>) <$> (((runProtoCallback prtClbk) $ d) $ wbF)
                          





