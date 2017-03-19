module Client.Neighbours where

import Types
import Packets
import Client.Crypto
import Client.Timer
import Client.Pipes
import Client.Ressource
import Client.WeedMonad


import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Lens
import qualified Data.Map as M

neighTimeOut = 20 :: Time 

-- | Regarde si le voisin est connu, si oui vérifie la signature du paquet 
-- | et appelle le bon callback sur le packet.
onNeighData ::  NeighData -> WeedMonad ()
onNeighData neighdata = do nMod <- stmRead clNeighbours
                           case neighID `M.lookup` nMod of
                                Nothing -> logM "Client.Neighbours" "onNeighData" InvalidPacket "Message received from unknown neighbours"
                                Just e -> if checkSig (_neighPubKey e) neighdata then onLayer2 neighID $ _neighDContent neighdata
                                          else logM "Client.Neighbours" "onNeighData" InvalidPacket "Invalid signature"
    where neighID = view neighDSource neighdata

onLayer2 :: UserID -> L2 -> WeedMonad ()
onLayer2 sID (L2Request req) = onRequest sID req
onLayer2 _ (L2Research res) = onResearch res
onLayer2 _ (L2Answer ans) = onAnswer ans

-- | Check the packet validity, and add the neighbours if it is unknown.
-- | If the neighbours is already present in the map, refresh the corresponding timer.
onNeighIntro ::  NeighIntro -> WeedMonad ()
onNeighIntro intro
    | not $ checkNeighIntro intro = logM "CLient.Neighbours" "onNeighIntro" InvalidPacket "Invalid signature"
    | otherwise = do nMap <- stmRead clNeighbours
                     case neighID `M.lookup` nMap of
                        Nothing -> do logM "Client.Neighbours" "onNeighIntro" Normal $ "New neighbour : " ++ show neighID
                                      stmModify clNeighbours . M.insert neighID . NeighEntry neighID (_neighIPubKey intro) =<< timer
                        Just e -> do logM "Client.Neighbours" "onNeighIntro" Normal $ "Refreshing known neighbour : " ++ show neighID
                                     refreshTimer (_neighTimerEntry e)
    where neighID = view neighISource intro
          timer = newTimer neighTimeOut $ removeNeighbour neighID


removeNeighbour :: UserID -> WeedMonad ()
removeNeighbour uID = do stmModify clNeighbours $ M.delete uID
                         logM "Client.Neighbours" "removeNeighbour" Normal $ "Neighbour removed : " ++ show uID

checkNeighIntro :: NeighIntro -> Bool
checkNeighIntro intro = checkSig pubkey intro && computeHashFromKey pubkey == _neighISource intro
    where pubkey = view neighIPubKey intro


