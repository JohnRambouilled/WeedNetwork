{-# LANGUAGE TemplateHaskell   #-}
module Types.Communication where

import Packets
import Types.Packets

import Control.Lens
import qualified Data.Map as M


data ComModule = ComModule {_comMap :: M.Map ComID ComEntry,
                            _comSource :: SourceID}

data ComEntry = ComEntry {comCallback :: ComMessage -> IO ()}


makeLenses ''ComModule
