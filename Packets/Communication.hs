{-# LANGUAGE DeriveGeneric #-}
module Packets.Communication where
import Data.Binary
import GHC.Generics
import Types.Crypto


data ComPacket = ComPinit ComInit | ComPmessage ComMessage
                 deriving (Generic, Show)
                          
data ComID = ComID Int
    deriving (Eq,Show,Generic,Ord)

data ComInit = ComInit {ciComID :: ComID, ciPayload :: RawData} -- ProtocolID ? 
                 deriving (Generic, Show)

data ComMessage = ComData {cmComID :: ComID, cmPayload :: RawData}
                | ComClose {cmComID :: ComID, cmPayload :: RawData}
                        deriving (Generic, Show)

isComExit :: ComMessage -> Bool
isComExit (ComClose _ _) = True
isComExit _ = False

instance Binary ComMessage
instance Binary ComInit
instance Binary ComID
instance Binary ComPacket
