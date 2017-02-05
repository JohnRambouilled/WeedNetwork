{-# LANGUAGE TemplateHaskell   #-}
module Types.Ressources where

import qualified Data.Map as M

import Packets
import Types.Packets
import Types.Crypto
import Types.Timer

import Control.Lens

type RessourcesMap = M.Map RessourceID RessourceEntry

data RessourceTilt = RessourceTilt {_tiltOn :: Bool,
                                    _tiltTimer :: TimerEntry}
                                    

data RessourceEntry = RessourceOffered {_ressourceAnswerContent :: RawData,
                                        _answerTilt :: RessourceTilt,
                                        _researchTilt :: RessourceTilt} |
                      RessourceResearched {_ressourceSources :: M.Map SourceID TimerEntry,
                                        _answerTilt :: RessourceTilt,
                                        _researchTilt :: RessourceTilt} 

makeLenses ''RessourceTilt
makeLenses ''RessourceEntry

