module Types.Destinary where

import Packets
import Types.Callbacks
import Types.Timer
import Types.Neighbours
import Types.Pipes
import Types.Communication

import Control.Concurrent.STM
import qualified Data.Map as M


data DestinaryID = DestinaryID
data DestinaryError = DestinaryError

data DestinaryEntry = DestinaryEntry {destinaryPipes :: [PipeID],
                                      destinaryTimer :: TimerEntry,
                                      destinaryCallback :: Callback DestinaryError PipeMessage, -- Commun à tous les pipes associés à la source
                                      destinaryComID :: TVar ComModule}
newtype DestinaryModule = DestinaryModule {destinaryControlMap :: M.Map DestinaryID DestinaryEntry}


