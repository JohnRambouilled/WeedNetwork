module Types.Neighbours where

import qualified Data.Map as M

import Types.Timer
import Types.Crypto
import Types.Packets

type NeighboursMap = M.Map NeighID NeighEntry

type NeighID = UserID

data NeighEntry = NeighEntry { neighEntryID :: NeighID, -- ????? TODO
                               neighTimerEntry :: TimerEntry,
                               neighPubKey :: PubKey}

