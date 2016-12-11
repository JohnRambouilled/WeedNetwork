module Types.Destinaries where

import Packets
import Types.Packets
import Types.Timer
import Types.Neighbours
import Types.Pipes
import Types.Communication
import Types.Crypto

import Control.Concurrent.STM
import qualified Data.Map as M

type DestinariesMap = M.Map DestinaryID DestinaryEntry

type DestinaryID = UserID
  
data DestinaryEntry = DestinaryEntry {destPipes :: [PipeID],
                                      destKeys :: PubKey,
                                      destComModule :: TVar ComMap}

