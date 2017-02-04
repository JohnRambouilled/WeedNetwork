module Types.Ressources where

import qualified Data.Map as M

import Packets
import Types.Packets

type RessourcesMap = M.Map RessourceID RessourceEntry

data RessourceEntry = RessourceOffered {ressourceAnswerContent :: RawData,
                                        ressourceTilt :: Bool} |
                      RessourceRelayed {ressourceSources :: M.Map SourceID TimerEntry,
                                        ressourceTilt :: Bool}
                    



{-
module Types.Ressources where

import Packets
import Types.Packets
import Types.Crypto
import Types.Timer

import Control.Concurrent.STM.TChan
import qualified Data.Map as M


type RessourcesMap = M.Map RessourceID RessourceEntry


data RessourceEntry = RessourceLocal { ressourceAnswerContent :: RawData} |
                      RessourceRelayed { ressourceSources :: M.Map SourceID RessourceSource,  --known sources offering this ressource
                                         ressourceTilt :: Bool} |  -- True if a research has been relayed recently (to prevent over-propagating researches)
                      RessourceDesired { ressourceSources :: M.Map SourceID RessourceSource,
                                         ressourceTChan :: TChan Answer, -- Broadcast TChan, use dupTChan to access content
                                         ressourceRelayAnswer :: Bool}



data RessourceSource = RessourceSource { resSourceCert :: RessourceCert,
                                         resSourceTimer :: TimerEntry }
                                         
-}
