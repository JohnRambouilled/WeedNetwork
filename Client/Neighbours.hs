module Client.Neighbours where

import Types
import Packets
import Client.Crypto
import Client.Timer


import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Lens
import qualified Data.Map as M


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

checkNeighIntro :: NeighIntro -> Bool
checkNeighIntro intro = checkSig pubkey intro && computeHashFromKey pubkey == neighIKeyID intro
    where pubkey = neighIPubKey intro


