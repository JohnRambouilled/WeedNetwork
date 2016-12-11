{-# LANGUAGE DeriveGeneric #-}
module Types.Protocol where

import Packets
import Types.Packets
import Types.Communication
import Types.Crypto

import GHC.Generics
import qualified Data.Map as M
import Data.Binary (Binary)
import Control.Concurrent.STM


newtype ProtocolID = ProtocolID Int
    deriving (Eq, Ord, Generic)

type ProtocolEntry = SourceID -> ComInit -> STM ComEntry

type ProtocolMap = M.Map ProtocolID ProtocolEntry

instance Binary ProtocolID


