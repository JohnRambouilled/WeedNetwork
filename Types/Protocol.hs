{-# LANGUAGE DeriveGeneric #-}
module Types.Protocol where

import Packets
import Types.Communication
import Types.Callbacks
import Types.Crypto

import GHC.Generics
import qualified Data.Map as M
import Data.Binary (Binary)


newtype ProtocolID = ProtocolID Int
    deriving (Eq, Ord, Generic)

type ProtocolEntry = RawData -> STMIO (Callback ComError ComMessage)


type ProtocolModule = M.Map ProtocolID ProtocolEntry

instance Binary ProtocolID


