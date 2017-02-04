{-# LANGUAGE TemplateHaskell   #-}
module Types.Ressources where

import qualified Data.Map as M

import Packets
import Types.Packets
import Types.Crypto
import Types.Timer

import Control.Lens

type RessourcesMap = M.Map RessourceID RessourceEntry

data RessourceTilt = RessourceTilt {_resTiltAns :: Bool, _resTiltRes :: Bool}

data RessourceEntry = RessourceOffered {_ressourceAnswerContent :: RawData,
                                        _ressourceTimer :: TimerEntry, 
                                        _ressourceTilt :: RessourceTilt} |
                      RessourceResearched {_ressourceSources :: M.Map SourceID TimerEntry,
                                           _ressourceTimer :: TimerEntry, 
                                           _ressourceTilt :: RessourceTilt}


makeLenses ''RessourceTilt
makeLenses ''RessourceEntry

