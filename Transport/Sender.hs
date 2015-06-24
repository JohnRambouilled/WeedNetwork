{-# LANGUAGE MultiParamTypeClasses #-}
module Transport.Sender where

import Control.Monad
import Control.Monad.State
import Packet
import Transport.Control
import Timer
import Class


data SendBuf = SendBuf { sbDataID :: DatagramID,
                         sbBuf    :: [RawData]}




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

