module Transport.Control where

{-| Protocol for receiving a given datagram :
       Every datagram segments stream must start with the segmentID 0
       Every datagram segments stream must finish with the flag flPush (acknowledged with a ACK message)
       Every segment must belong to the current datagram. If it is not the case, it is discarded
       and some GET messages are sent.

       Acknowledges every PSH concerning previous datagrams

       You MUST NOT send a new datagram when the previous flPush has not been acknowledged
-}
       

import Packet

import Data.List.Ordered
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Maybe
import Data.Binary


type DatagramID = Int
type SegmentID = Int

data Flags = Flags {flPush :: Bool}
data TrSegment = TrSegment { trDatagramID :: DatagramID,
                             trSegmentID  :: SegmentID,
                             trFlags      :: Flags,
                             trData       :: RawData}
{- Infinite segment is -1 -}
data TrControlMessage =  -- Asks for a range of segments
                         TrGet { cmDataID :: DatagramID,
                                 cmSegRequests :: [SegmentRequest]}
                         -- Acknowledges a datagram
                      | TrAck { cmAckID :: DatagramID}
instance Eq TrSegment where
        t1 == t2 = trDatagramID t1 == trDatagramID t2
                && trSegmentID t1 == trSegmentID t2
             
instance Ord TrSegment where
        t1 `compare` t2 = case cmpDgram of
                            EQ -> trSegmentID t1 `compare` trSegmentID t2
                            _ -> cmpDgram
              where cmpDgram = trDatagramID t1 `compare` trDatagramID t2


data SegmentRequest = SegmentRange { fromSegment :: SegmentID,
                                     toSegment   :: SegmentID}
                    | SegmentFrom {fromSegment :: SegmentID}

---------------------------8<----------------------------------------
--          Receiver
------------------------------------------------------------------
extractRanges :: [SegmentID] -> [(SegmentID,SegmentID)]
extractRanges [] = []
extractRanges l = catMaybes $ zipWith f l (tail l)
        where f el nxt
                | el+1 == nxt = Nothing
                | otherwise  = Just (el,nxt)

instance Binary Flags where
        put (Flags f) = put f
        get = Flags <$> get

instance Binary SegmentRequest where
        put (SegmentRange from to) = putWord8 0 >> put from >> put to
        put (SegmentFrom from) = putWord8 1 >> put from
        get = do x <- getWord8 
                 case x of
                   0 -> SegmentRange <$> get <*> get 
                   1 -> SegmentFrom <$> get
                   _ -> fail "invalid segmentrequest"
           
instance Binary TrSegment where
        put (TrSegment dID sID flags raw) =  put dID >> put sID >> put flags >> put raw
        get = TrSegment <$> get <*> get <*> get <*> get

instance Binary TrControlMessage where
        put (TrGet dID req) = putWord8 0 >> put dID >> put req
        put (TrAck x) = putWord8 1 >> put x
        get = do x <- getWord8 
                 case x of
                   0 -> TrGet <$> get <*> get
                   1 -> TrAck <$> get
                   _ -> fail "invalid transport control message"

