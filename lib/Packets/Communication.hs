{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell   #-}
module Packets.Communication where
import Types.Crypto
import Types.Packets

import Data.Binary
import GHC.Generics
import Control.Lens


data ComPacket = ComPinit ComInit | ComPmessage ComMessage
                 deriving (Generic, Show)
                          
data ComID = ComID Int
    deriving (Eq,Show,Generic,Ord)

data ComInit = ComInit {_ciComID :: ComID, 
                        _ciProtocolID :: ProtocolID, 
                        _ciPayload :: RawData}
                 deriving (Generic, Show)

data ComMessage = ComData {_cmComID :: ComID, _cmPayload :: RawData}
                | ComClose {_cmComID :: ComID, _cmPayload :: RawData}
                        deriving (Generic, Show)


makeLenses ''ComInit
makeLenses ''ComMessage

isComExit :: ComMessage -> Bool
isComExit (ComClose _ _) = True
isComExit _ = False

instance Binary ComMessage
instance Binary ComInit
instance Binary ComID
instance Binary ComPacket
