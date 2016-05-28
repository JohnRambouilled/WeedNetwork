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

checkRequest :: UserID -> Time -> Request -> (Either String Request)
checkRequest = checkReq
    where checkReq me t req@(Request n l r epk t' pK pH s c)
            | l > roadLengthMax                 = Left "Rejected road : too long"
            | n > l-1                           = Left "Incorrect RequestPosition"
            | l /= length r                      = Left "Incorrect RoadLength"
            | t - t' > maxDelay                 = Left "Obsolete Request" 
            | r !! n /= me                       = Left "Not adressed to me"
            | computeHashFromKey pK /= pH        = Left "Invalid tuple KeyHash/PubKey"
            | checkSig pK req                   = Right req
            | otherwise                         = Left "Invalid signature"

