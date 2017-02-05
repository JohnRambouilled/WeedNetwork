{-# LANGUAGE TemplateHaskell   #-}
module Types.Neighbours where

import qualified Data.Map as M

import Types.Timer
import Types.Crypto
import Types.Packets

import Control.Lens

type NeighboursMap = M.Map NeighID NeighEntry

type NeighID = UserID

data NeighEntry = NeighEntry { _neighEntryID :: NeighID, -- ????? TODO
                               _neighPubKey :: PubKey,
                               _neighTimerEntry :: TimerEntry}

makeLenses ''NeighEntry

