{-# LANGUAGE TemplateHaskell #-}
module Types.Pipes where

import Types.Crypto
import Types.Callbacks
import Types.Neighbour
import Packets.Neighbours
import Packets.PipePackets

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Lens
import qualified Data.Map as M


newtype SourceID = SourceID Int
data PipeError = PipeTimedOut | PipeBroken | PipeClosedError Payload

data PipeEntry = PipeEntry {_pipeCallback :: Callback PipeError PipeMessage,
                            _pipePubKey :: PubKey,
                            _pipeTimer :: TimerEntry,
                            _pipeNodes :: (NeighID, NeighID)}
makeLenses ''PipeEntry

-- Contiens tout les pipes (relayés, leech et seed)
type PipesModule = M.Map PipeID PipeEntry


registerCallback :: TVar PipesModule -> PipeID -> PubKey -> Callback PipeError PipeMessage -> (NeighID, NeighID) -> STMIO ()
registerCallback pipesModule pipeID pubkey callback nodes = do timer <- newTimerEntry kill
                                                               let entry = PipeEntry callback pubkey timer nodes 
                                                               lift $ modifyTVar pipesModule (M.insert pipeID entry) 
            where kill :: STMIO ()
                  kill = do pipeM <- deleteLookup pipeID pipesModule
                            case pipeM of
                                Nothing -> error "PipeModule : timeout d'une entrée non présente"
                                Just pipeE -> closeWithError callback PipeTimedOut

onNeighBreak :: TVar PipesModule -> (NeighID,NeighBreak) -> STMIO ()
onNeighBreak pipesModule (neighID, neighbreak) = do pipesMod <- lift $ readTVar pipesModule
                                                    forM_ (neighBPipes neighbreak) (f pipesMod) 
    where f pipesMod pipeID = case pipeID `M.lookup` pipesMod of 
                                        Nothing -> pure ()
                                        Just pipeE -> let nodes = _pipeNodes pipeE
                                                     in when (neighID == fst nodes || neighID == snd nodes) $ removePipe pipesModule pipeID PipeBroken
--
removePipe :: TVar PipesModule -> PipeID -> PipeError -> STMIO ()
removePipe pipesModule pipeID pipeError = do pipeM <- deleteLookup pipeID pipesModule
                                             case pipeM of
                                                Nothing -> error "PipeModule : on essaye de supprimer une entrée non présente"
                                                Just pipeE -> do closeWithError (_pipeCallback pipeE) pipeError
                                                                 timerKill $ _pipeTimer pipeE


onPipePacket :: TVar PipesModule -> PipePacket -> STMIO ()
onPipePacket pipesModule pipePacket = do pipesMod <- lift $ readTVar pipesModule
                                         case pipeID `M.lookup` pipesMod of
                                            Nothing -> pure ()
                                            Just pipeEntry -> when (checkSig (_pipePubKey pipeEntry) pipePacket) $ onPipePacket' pipeEntry
    where pipeID = pipeKeyID pipePacket
          onPipePacket' pipeEntry = case pipePacket of
                                        PipePacket _ _ _ _ payload -> call (_pipeCallback pipeEntry) (pipeID,payload)
                                        PipeClose _ _ _ _ payload -> do closeWithError (_pipeCallback pipeEntry) (PipeClosedError payload)
                                                                        lift $ modifyTVar pipesModule $ M.delete pipeID
                                                                        timerKill $ _pipeTimer pipeEntry
