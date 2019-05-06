module Types.Protocol where

import Packets
import Types.Packets
import Types.Communication
import Types.Crypto

import GHC.Generics
import qualified Data.Map as M
import Data.Binary (Binary)
import Control.Concurrent.STM



type ProtocolEntry = SourceID -> RawData -> STM ComEntry

type ProtocolMap = M.Map ProtocolID ProtocolEntry



