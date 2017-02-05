module Client.Neighbours where

import Types
import Packets
import Client.Crypto
import Client.Timer
import Client.Pipes
import Client.Ressoures


import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Lens
import qualified Data.Map as M

neighTimeOut 

-- Regarde si le voisin est connu, si oui vérifie la signature du paquet 
onNeighData ::  NeighData -> WeedMonad ()
onNeighData neighdata = do nMod <- stmRead clNeighbours
                           case neighID `M.lookup` nMod of
    where neighID = neighDKeyID neighdata
          managePacket entry = if checkSig (neighPubKey entry) neighdata then Just neighData else Nothing 

-- | Check the packet validity, and add the neighbours if it is unknown.
-- | If the neighbours is already present in the map, refresh the corresponding timer.
onNeighIntro ::  NeighIntro -> WeedMonad Bool
onNeighIntro neighbourMod intro
    | not $ checkNeighIntro intro = pure False
    | otherwise = do nMap <- stmRead clNeighbours
                     case neighID `M.lookup` nMap of
                        Nothing -> stmModify clNeighbours . M.insert neighID (_neighIPubKey intro) =<< timer
                        Just e -> refreshTimer (_neighTimerEntry e)
    where neighID = neighIKeyID intro
          timer = newTimerEntry neighTimeOut $ removeNeighbourg neighID


removeNeighbour

checkNeighIntro :: NeighIntro -> Bool
checkNeighIntro intro = checkSig pubkey intro && computeHashFromKey pubkey == neighIKeyID intro
    where pubkey = view neighIPubKey intro


