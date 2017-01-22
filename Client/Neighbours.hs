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


-- Regarde si le voisin est connu, si oui vérifie la signature du paquet 
onNeighData ::  NeighData -> WeedMonad ()
onNeighData neighdata = do nMod <- stmRead clNeighbours
                           case neighID `M.lookup` nMod of
    where neighID = neighDKeyID neighdata
          managePacket entry = if checkSig (neighPubKey entry) neighdata then Just neighData else Nothing 

-- Vérifie la validité du paquet et ajoute le voisin
onNeighIntro ::  NeighIntro -> WeedMonad Bool
onNeighIntro neighbourMod intro
    | not $ checkNeighIntro intro = pure False
    | otherwise = do timerEntry <- newTimerEntry $ stmModify neighbourMod kill
                     nMod <- stmRead neighbourMod
                     let (entryM,nMap') = M.insertLookupWithKey (\_ _ old -> old) neighID neighEntry (_neighControlMap nMod)
                         neighEntry = NeighEntry neighID timerEntry (neighIPubKey intro)
                     stmWrite neighbourMod $ set neighControlMap nMap' nMod
                     forM_ entryM (timerRefresh . neighTimerEntry)
    where neighID = neighIKeyID intro
          kill = over neighControlMap (M.delete neighID) 

checkNeighIntro :: NeighIntro -> Bool
checkNeighIntro intro = checkSig pubkey intro && computeHashFromKey pubkey == neighIKeyID intro
    where pubkey = neighIPubKey intro


