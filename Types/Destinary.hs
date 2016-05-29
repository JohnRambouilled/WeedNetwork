module Types.Destinary where

import Packets
import Types.Callbacks
import Types.Timer
import Types.Neighbours
import Types.Pipes
import Types.Communication
import Types.Crypto
import Types.Sendable

import Control.Concurrent.STM
import qualified Data.Map as M


type PipeSender = RawData -> STMIO ()
  


type DestinaryID = UserID
data DestinaryEntry = DestinaryEntry {destinaryPipes :: M.Map PipeID PipeSender,
                                      destinaryDHPubKey :: DHPubKey,
                                      destinarySender :: PipeSenderOption -> PipeSender,
                                      destinaryComModule :: TVar ComModule}
type DestinaryModule =  M.Map DestinaryID DestinaryEntry


