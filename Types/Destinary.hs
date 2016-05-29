module Types.Destinary where

import Packets
import Types.Callbacks
import Types.Timer
import Types.Neighbours
import Types.Pipes
import Types.Communication
import Types.Crypto

import Control.Concurrent.STM
import qualified Data.Map as M



type DestinaryID = UserID
data DestinaryEntry = DestinaryEntry {destinaryPipes :: [PipeID],
                                      destinaryKeyPair ::  KeyPair,
                                      destinaryComModule :: TVar ComModule}
type DestinaryModule =  M.Map DestinaryID DestinaryEntry


