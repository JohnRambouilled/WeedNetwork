{-# LANGUAGE TemplateHaskell #-}
module Types.Neighbours where

import Control.Lens
import qualified Data.Map as M

import Types.Timer
import Types.Callbacks
import Types.Crypto
import Packets

type NeighID = UserID


data NeighError = NeighError

data NeighEntry = NeighEntry { neighEntryID :: NeighID, -- ????? TODO
                               neighTimerEntry :: TimerEntry,
                               neighPubKey :: PubKey}
data NeighbourModule = NeighbourModule {_neighControlMap :: M.Map NeighID NeighEntry,
                                        _neighRequestCb :: Callback NeighError (NeighID,Request),
                                        _neighRessourceCb :: Callback NeighError RessourcePacket,
                                        _neighBreakCb :: Callback NeighError (NeighID,NeighBreak)
                                        }
makeLenses ''NeighbourModule

