module Types.Neighbour where

import Control.Concurrent.STM
import qualified Data.Map as M


type UserID = Int
type PubKey = Int
data NeighID = NeighID {neighID :: UserID}

-- Paquets
data NeighBreak = NeighBreak
data Request = Request
data RessourcePkt = RessourcePkt

type TimerRefresh = IO ()
type TimerKill = IO ()
data TimerEntry = TimerEntry {timerRefresh :: TimerRefresh,
                              timerKill :: TimerKill}

data NeighEntry = NeighEntry { neighEntryID :: NeighID, -- ????? TODO
                               neighTimerEntry :: TimerEntry,
                               neighPubKey :: PubKey}
data NeighbourModule = NeighbourModule {neighControlMap :: M.Map NeighID NeighEntry,
                                        neighRequestTChan :: TChan Request,
                                        neighRessourceTChan :: TChan RessourcePkt,
                                        neighBreakTChan :: TChan NeighBreak
                                        }

-- Private
-- Action atomique a faire pour chaque packet. Retourne les éventuelles actions IO a executer (ex : register timer, refresh...)
--onNeighPacket :: TVar NeighbourModule -> Either Introduce NeighPacket -> STM (Maybe (IO()) )

-- Public
--
-- Packet = Introduce | NeighPacket | PipePacket
-- NeighPacket = Request | RessourcePkt | NeighBreak
-- PipePacket = PipeID PipeMessage Signature

-- Retourne une map synchronisée par un thread sur les flux entrant
--newNeighbourModule :: TChan Introduce -> TChan NeighPacket -> IO (TVar NeighbourModule)

-- Retire proprement une entrée (kill le timer, envoi un neighBreak, et clean l'entry)
--closeNeighEntry :: TVar NeighbourModule -> NeighID -> IO ()
--
--
--
