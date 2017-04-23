{-# LANGUAGE TemplateHaskell #-}
module Types.Pipes where

import Packets
import Types.Packets
import Types.Crypto
import Types.Timer
import Types.Neighbours
import Types.Communication

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import qualified Data.Map as M

type RelayedPipesMap = M.Map PipeID RelayedPipeEntry
type LocalPipeMap = M.Map PipeID LocalPipeEntry

data RelayedPipeEntry = RelayedPipeEntry {_relPipePubKey :: PipePubKey,
                                          _relPipePrevious :: UserID,
                                          _relPipeNext :: UserID,
                                          _relPipeTimer :: TimerEntry}

data LocalPipeEntry = LocalPipeEntry {_locPipeNeigh :: UserID,
                                      _locPipeTimer :: TimerEntry,
                                      _locPipeRepeater :: Maybe TimerEntry,
                                      _locPipeSource :: SourceID}



makeLenses ''RelayedPipeEntry
makeLenses ''LocalPipeEntry


