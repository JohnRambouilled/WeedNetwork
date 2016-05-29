{-# LANGUAGE DeriveGeneric #-}
module Packets.Communication where
import Data.Binary
import GHC.Generics
import Types.Crypto


data ComPacket = ComPinit ComInit | ComPmessage ComMessage

data ComID = ComID Int
    deriving (Eq,Show,Generic,Ord)
data ComInit = ComInit {ciComID :: ComID, ciPayload :: RawData} -- ProtocolID ? 
             deriving Generic
data ComMessage = ComData {cmComID :: ComID, cmPayload :: RawData}
                | ComClose {cmComID :: ComID, cmPayload :: RawData}
                            deriving Generic



isComExit (ComClose _ _) = True
isComExit _ = False
instance Binary ComMessage
instance Binary ComInit
instance Binary ComID
