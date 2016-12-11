{-# LANGUAGE TemplateHaskell #-}
module Types.Pipes where

import Packets
import Types.Packets
import Types.Crypto
import Types.Timer
import Types.Neighbours
import Types.Communication

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import qualified Data.Map as M

type RelayedPipesMap = M.Map PipeID RelayedPipeEntry
type IncomingPipesMap = M.Map PipeID IncomingPipeEntry
type OutgoingPipesMap = M.Map PipeID OutgoingPipeEntry

data RelayedPipeEntry = RelayedPipeEntry {relPipePubKey :: PipePubKey,
                                          relPipePosition :: Number,
                                          relPipeTimer :: TimerEntry}

data IncomingPipeEntry = IncomingPipeEntry {incPipeKeys :: PipeKeyPair,
                                            incPipeTimer :: TimerEntry,
                                            incPipeComMap :: TVar ComMap}

data OutgoingPipeEntry = OutgoingPipeEntry {outPipeKeys :: PipeKeyPair,
                                            outPipeTimer :: TimerEntry,
                                            outPipeComMap :: TVar ComMap}




