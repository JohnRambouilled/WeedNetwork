{-# LANGUAGE TemplateHaskell,MultiParamTypeClasses #-}
module Transport.Sender where

import Data.Binary hiding (get)
import Client.Protocol
import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Control.Monad.State
import Client.Packet
import Transport.Control
import Timer
import Log
import Client.Class

import Control.Lens

fragSize = 1000 --- TODO

data SendBuf = SendBuf { sbDataID :: DatagramID,
                         sbBuf    :: [RawData]} |
               BufKill RawData deriving Show


data Sender = Sender {_sToSend :: [SendBuf],
                      _sKill :: IO (),
                      _sCurrentDatagram :: DatagramID}

makeLenses ''Sender


senderQueueDatagram :: (MonadIO m) => MVar Timer ->  (WriteFun, BreakFun) -> IO () -> RawData -> StateT Sender m Bool
senderQueueDatagram timerV wF break raw  = do curID <- use sCurrentDatagram
                                              keepL Normal $ "adding new Datagram to queue : " ++ show curID
                                              sCurrentDatagram += 1
                                              senderQueueSendBuf timerV wF break $ SendBuf (curID + 1) raws
    where raws = fragData raw


senderQueueSendBuf :: (MonadIO m) => MVar Timer ->  (WriteFun, BreakFun) -> IO () -> SendBuf -> StateT Sender m Bool
senderQueueSendBuf timerV wF break buf = do toSend <- use sToSend
                                            keepL Normal "adding buffer to queue"
                                            sToSend .= toSend ++ [buf]
                                            if null toSend 
                                                then do keepL Normal "No datagram in the buffer, sending new datagram"
                                                        and `fmap` (senderSendBuf timerV buf wF break >>= mapM (sendTRSegment $ fst wF))
                                                else do keepL Normal "Datagram added to queue"
                                                        return True



runSendBuf :: (MonadIO m) => TrControlMessage -> StateT SendBuf m (Maybe [TrSegment])
runSendBuf msg = (onControlMessage <$> sbDataID <*> sbBuf <*> pure msg) <$> get


{-| The buffer MUST be ordered in raising segnum order |-}
buildSegments :: DatagramID -> [(Int,RawData)] -> [TrSegment]
buildSegments bufID raws = foldr f [] raws
  where f (ind,raw) [] = [TrSegment bufID ind (Flags True) raw]
        f (ind,raw) l = TrSegment bufID ind (Flags False) raw:l

onSegRequest :: DatagramID -> [RawData] -> SegmentRequest -> [TrSegment]
onSegRequest bufID buf (SegmentRange from to) = buildSegments bufID $ take (to - from + 1) $ drop from (zip [0..] buf)
onSegRequest bufID buf (SegmentFrom from) = buildSegments bufID $ drop from $ zip [0..] buf


onControlMessage :: DatagramID -> [RawData] -> TrControlMessage -> Maybe [TrSegment]
onControlMessage _ [] (TrGet dID req) = Just []
onControlMessage bufID buf (TrGet dID req) = if dID == bufID then Just $ req >>= onSegRequest bufID buf 
                                                                 else Just []
onControlMessage _ [] (TrAck dID) = Just []
onControlMessage bufID buf (TrAck dID) = if dID == bufID then Nothing
                                                        else Just []


senderSendBuf :: (MonadIO m) => MVar Timer -> SendBuf -> (WriteFun, BreakFun) -> IO () -> StateT Sender m [TrSegment]
senderSendBuf _ (BufKill d) (_,bF) break = do keepL Important "[senderSendBuf] BufKill received, closing communication."
                                              liftIO $ break >> (runBreakFun bF $ d)
                                              pure []
senderSendBuf timerV buf (wF,_) break = let ret = buildSegments (sbDataID buf) $ zip [0..] $ sbBuf buf
                                    in do killRepeat <- liftIO $ repeatNTimes timerV (void $ spamPsh (last ret)) break  pshFreq pshRepeatParam 
                                          keepL Normal "[senderSendBuf] Sending Datagramm"
                                          bufs <- use sToSend
                                          sKill .= killRepeat
                                          return ret
        where pshFreq = 5
              pshRepeatParam = 3
              spamPsh payload = sendTRSegment wF payload

fragData :: RawData -> [RawData]
fragData raw = fst $ until (B.null . snd) f ([],raw)
  where f (frags,rest) = let (nfrag,nrest) = B.splitAt fragSize rest
                         in (frags ++ [nfrag],nrest)

sendTRSegment :: MonadIO m => WriteFun -> TrSegment -> m Bool
sendTRSegment wF = liftIO . runWriteFun wF . encode . TransportSeg


