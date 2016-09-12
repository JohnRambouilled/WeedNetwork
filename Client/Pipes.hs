module Client.Pipes where

import Types
import Packets
import Client.Crypto
import Client.Timer

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Lens
import qualified Data.Map as M


{- Insère un callback suite à une requête. Attention, la position doit être vérifiée. De plus, la position 1 signifie que nous en sommes l'origine -}
registerPipe :: TVar PipesModule -> Request -> Callback PipeError PipeData -> STMIO ()
registerPipe pipesModule (Request pos len road epk time pubKey pipeID sig payload) callback = registerCallback pipesModule pipeID pubKey callback pos nodes
    where nodes = if pos == 1 then PipeEnd (road !! pos)
                    else if pos == len - 1 then PipeEnd $ road !! (len - 2)
                    else PipeNode (road !! (pos - 1)) (road !! (pos +1))

registerCallback :: TVar PipesModule -> PipeID -> PubKey -> Callback PipeError PipeData -> Number -> PipeKind -> STMIO ()
registerCallback pipesModule pipeID pubkey callback number nodes = do timer <- newTimerEntry kill
                                                                      let entry = PipeEntry callback pubkey timer number nodes 
                                                                      stmModify pipesModule (M.insert pipeID entry) 
            where kill :: STMIO ()
                  kill = do (pipeM, pipeMap') <- deleteLookup pipeID <$> stmRead pipesModule
                            stmWrite pipesModule pipeMap'
                            case pipeM of
                                Nothing -> error "PipeModule : timeout d'une entrée non présente"
                                Just pipeE -> closeWithError callback PipeTimedOut

onNeighBreak :: TVar PipesModule -> (NeighID,NeighBreak) -> STMIO ()
onNeighBreak pipesModule (neighID, neighbreak) = do pipesMod <- stmRead pipesModule
                                                    forM_ (neighBPipes neighbreak) (f pipesMod) 
    where f pipesMod pipeID = case pipeID `M.lookup` pipesMod of 
                                        Nothing -> pure ()
                                        Just pipeE -> case _pipeKind pipeE of
                                                        PipeNode prev next -> when (neighID == prev || neighID == next) $ removePipe pipesModule pipeID PipeBroken
                                                        PipeEnd next -> when (neighID ==next) $  removePipe pipesModule pipeID PipeBroken
--
removePipe :: TVar PipesModule -> PipeID -> PipeError -> STMIO ()
removePipe pipesModule pipeID pipeError = do (pipeM,pipeMap') <- deleteLookup pipeID <$> stmRead pipesModule
                                             stmWrite pipesModule pipeMap'
                                             case pipeM of
                                                Nothing -> error "PipeModule : on essaye de supprimer une entrée non présente"
                                                Just pipeE -> do closeWithError (_pipeCallback pipeE) pipeError
                                                                 timerKill $ _pipeTimer pipeE


onPipePacket :: TVar PipesModule -> PipePacket -> STMIO ()
onPipePacket pipesModule pipePacket = do pipesMod <- stmRead pipesModule
                                         case pipeID `M.lookup` pipesMod of
                                            Nothing -> pure ()
                                            Just pipeEntry -> when (checkPacket pipeEntry) $ onPipePacket' pipeEntry
    where pipeID = scKeyHash pipePacket
          checkPacket pipeEntry  = checkSig (_pipePubKey pipeEntry) pipePacket 
                                && _pipeEntryPosition pipeEntry == (case pipePacket of
                                                                             PipePacketControl (PipeControl _ _ n _ _) -> n
                                                                             PipePacketData (PipeData _ _ n _ _) -> n)
                                      
          onPipePacket' pipeEntry = case pipePacket of
                                        PipePacketData pkt@(PipeData _ _ _ _ _) -> call (_pipeCallback pipeEntry) pkt
                                        PipePacketControl (pkt@(PipeControl _ _ _ _ (PipeClose _) ))-> do closeWithError (_pipeCallback pipeEntry) (PipeClosedError pkt)
                                                                                                          stmModify pipesModule $ M.delete pipeID
                                                                                                          timerKill $ _pipeTimer pipeEntry
                                        PipePacketControl (pkt@(PipeControl _ _ _ _ PipeControlRefresh)) ->  timerRefresh $ _pipeTimer pipeEntry
                                        otherwise -> logM $ RawLog "[PIPES] Message de contrôle inconnu"


