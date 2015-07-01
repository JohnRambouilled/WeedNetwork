module Transport.Receiver where

import Transport.Control
import Control.Monad
import Control.Monad.State
import Data.List.Ordered

import Debug.Trace

newtype RecvBuf = RecvBuf {rbBuf :: [TrSegment]}



                        

{-| Inserts a new segment in the buffer if the datagramID is the same.
    Checks the buffer and returns the list of missing segments if it is not. |-}
receiveFlow :: [TrSegment] -> TrSegment -> Either TrControlMessage [TrSegment] 
receiveFlow [] x = Right [x]
receiveFlow l@(x:_) x'
    | trDatagramID x == trDatagramID x' = Right $ insertSet x' l
    | trDatagramID x < trDatagramID x' = Left $ TrGet  (trDatagramID x) $ SegmentFrom (trSegmentID x + 1) :
                                                                                      (craftReqRangePkt $ checkBuf l)
    | otherwise                        = Left $ TrAck (trDatagramID x')

craftReqRangePkt :: [(SegmentID,SegmentID)] -> [SegmentRequest]
craftReqRangePkt = map (uncurry SegmentRange) 


{-| Checks the buffer and returns the list of missing segments. |-}
checkBuf :: [TrSegment] -> [(SegmentID,SegmentID)]
checkBuf [] = []
checkBuf buf@(lastSeg:_) = extractRanges $ trace "minus..." $ minus (trace "wanted" wantedIndexes) (trace "received" receivedIndexes)
  where receivedIndexes = map trSegmentID buf
        wantedIndexes = [sID | sID <- [0.. lastSegID]] 
        lastSegID = trSegmentID lastSeg


{-| Inserts a push packet into the buffer when it corresponds to the specified datagram.
    Checks the buffer and returns the list of missing segments.
    Right : ack packet to send, transmission properly signed |-}
onPshPacket :: [TrSegment] -> TrSegment -> Either ([TrSegment], Maybe TrControlMessage) ([TrSegment],TrControlMessage)
onPshPacket buf seg = case trace "receiveFlow : " $ receiveFlow buf seg of
                        Left err -> Left (buf,pure err)
                        Right v -> let missings = checkBuf (trace "haha" v) --(v,map (craftPkt $ trDatagramID seg) $ checkBuf v)
                                  in if trace ("missings : ") $ null missings then Right (v,TrAck $ trDatagramID seg)
                                                      else Left (v, pure $ TrGet (trDatagramID seg) $ craftReqRangePkt missings)


runRecvBuf :: [TrSegment] -> TrSegment -> Either ([TrSegment],Maybe TrControlMessage) ([TrSegment],TrControlMessage)
runRecvBuf buf seg
    | flPush $ trFlags seg  = trace "PshPacket " $ onPshPacket buf seg
    | otherwise           = Left $ case receiveFlow buf seg of
                                     Left err -> (buf,Just err)
                                     Right v -> (v,Nothing)
------------------------------------------------------------------------


