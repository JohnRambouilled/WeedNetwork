{-# LANGUAGE TemplateHaskell #-}
module Types.Neighbour where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Lens
import qualified Data.Map as M

import Types.Callbacks
import Types.Crypto
import Packets.PipePackets
import Packets.Neighbours
import Packets.Ressource

type NeighID = UserID


data NeighError = NeighError

type TimerRefresh = STMIO ()
type TimerKill = STMIO ()
data TimerEntry = TimerEntry {timerRefresh :: TimerRefresh,
                              timerKill :: TimerKill}

newTimerEntry :: STMIO () -> STMIO TimerEntry
newTimerEntry kill = pure $ TimerEntry (pure ()) (pure ()) 

data NeighEntry = NeighEntry { neighEntryID :: NeighID, -- ????? TODO
                               neighTimerEntry :: TimerEntry,
                               neighPubKey :: PubKey}
data NeighbourModule = NeighbourModule {_neighControlMap :: M.Map NeighID NeighEntry,
                                        _neighRequestCb :: Callback NeighError Request,
                                        _neighRessourceCb :: Callback NeighError RessourcePacket,
                                        _neighBreakCb :: Callback NeighError (UserID,NeighBreak)
                                        }
makeLenses ''NeighbourModule

-- Private
-- Regarde si le voisin est connu, si oui vérifie la signature du paquet et appele le callback correspondant
onNeighPacket :: TVar NeighbourModule ->  NeighData -> STMIO ()
onNeighPacket neighbourMod neighdata = do nMod <- lift $ readTVar neighbourMod
                                          forM_ ((neighID `M.lookup`) $ _neighControlMap nMod) (managePacket nMod)
    where neighID = neighDKeyID neighdata
          managePacket nMod entry = when (checkSig (neighPubKey entry) neighdata) $ 
                                        case neighDContent neighdata of
                                            NeighReq req -> call (_neighRequestCb nMod) req
                                            NeighRes res -> call (_neighRessourceCb nMod) res
                                            NeighBrk brk -> do when (neighID == neighBOrigin brk) $ lift $ writeTVar neighbourMod (over neighControlMap (M.delete neighID) nMod) 
                                                               call (_neighBreakCb nMod) (neighID,brk)

-- Vérifie la validité du paquet et ajoute le voisin
onIntroduce :: TVar NeighbourModule ->  NeighIntro -> STMIO ()
onIntroduce neighbourMod intro
    | not $ checkNeighIntro intro = pure ()
    | otherwise = do timerEntry <- newTimerEntry $ lift $ modifyTVar neighbourMod kill
                     nMod <- lift $ readTVar neighbourMod
                     let (entryM,nMap') = M.insertLookupWithKey (\_ _ old -> old) neighID neighEntry (_neighControlMap nMod)
                         neighEntry = NeighEntry neighID timerEntry (neighIPubKey intro)
                     lift $ writeTVar neighbourMod $ set neighControlMap nMap' nMod
                     forM_ entryM (timerRefresh . neighTimerEntry)
    where neighID = neighIKeyID intro
          kill = over neighControlMap (M.delete neighID) 

checkNeighIntro intro = checkSig pubkey intro && computeHashFromKey pubkey == neighIKeyID intro
    where pubkey = neighIPubKey intro



-- Public
--
-- Packet = Introduce | NeighPacket | PipePacket
-- NeighPacket = Request | RessourcePkt | NeighBreak
-- PipePacket = PipeID PipeMessage Signature

-- Retourne une map synchronisée par un thread sur les flux entrant
--newNeighbourModule :: IO (TVar NeighbourModule)

-- Retire proprement une entrée (kill le timer, envoi un neighBreak, et clean l'entry)
--closeNeighEntry :: TVar NeighbourModule -> NeighID -> IO ()
--
--
--
