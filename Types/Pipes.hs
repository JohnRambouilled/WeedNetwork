{-# LANGUAGE TemplateHaskell #-}
module Types.Pipes where

import Packets
import Types.Crypto
import Types.Timer
import Types.Callbacks
import Types.Neighbours

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Lens
import qualified Data.Map as M


newtype SourceID = SourceID Int
data PipeError = PipeTimedOut | PipeBroken | PipeClosedError Payload

data PipeEntry = PipeEntry {_pipeCallback :: Callback PipeError PipeMessage,
                            _pipePubKey :: PubKey,
                            _pipeTimer :: TimerEntry,
                            _pipeNodes :: (NeighID, NeighID)}
makeLenses ''PipeEntry

-- Contiens tout les pipes (relayés, leech et seed)
type PipesModule = M.Map PipeID PipeEntry


