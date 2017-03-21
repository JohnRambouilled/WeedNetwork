module Client.Destinary where

import Types
import Packets
import Client.Crypto
import Client.Communication
import Client.WeedMonad
import Client.Timer
import Client.Sender

import Control.Lens
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Binary (decodeOrFail)

destinaryTimeOut = 60 :: Time

-- | open a Pipe on a given Road. Require the Public Key of the destinary.
openPipe :: Road -> RawData -> PubKey -> WeedMonad Bool
openPipe r cnt sK = do dEM <- destinaryInsertPipes sK sID [pID] 
                       case dEM of
                            Nothing -> return False
                            Just dE -> let pipeK = fst $ view destPipeKeys dE in sendRequest r pipeK cnt >> pure True
  where pID = computePipeID r
        sID = last r

-- |  Remove a pipe in the destinary pipeMap. If it is the only pipe leading to that source, it start the timeOut timer.
destinaryRemovePipe :: UserID -> PipeID -> WeedMonad ()
destinaryRemovePipe sID pID = do destMod <- stmRead clDestinaries
                                 case sID `M.lookup` destMod of
                                    Nothing -> logM "Client.Destinary" "destinaryRemovePipe" Error "Attempted to remove a pipe from a non-existing source" >> pure ()
                                    Just e -> do stmWrite clDestinaries $ M.insert sID (over destPipes (filter (==pID)) e) destMod
                                                 when (_destPipes e == [pID]) $ startTimer (_destTimer e)



-- | Add the requested pipe in the Destinary map, and create a new DestinaryEntry if necessary.
-- | Returns Nothing if Diffie-Hellman fails (for a new source), or if the key present in the request does not match the one stored for the source.
-- | Else return the source entry.
-- | If the destinary had no other pipes, kills the timeOut timer.
destinaryInsertPipes :: PubKey -> UserID -> [PipeID] -> WeedMonad (Maybe DestinaryEntry)
destinaryInsertPipes pk sID pIDs = do destMod <- stmRead clDestinaries
                                      case sID `M.lookup` destMod of
                                            Nothing -> do destEM <- newDestinaryEntry sID pk pIDs
                                                          case destEM of
                                                              Nothing -> pure Nothing
                                                              Just destE -> do stmModify clDestinaries $ M.insert sID destE
                                                                               pure $ Just destE
                                            Just destE -> if pk == _destKey destE then do let destE' = over destPipes (pIDs++) destE
                                                                                          stmWrite clDestinaries $ M.insert sID destE' destMod
                                                                                          when (null $ view destPipes destE) $ killTimer (view destTimer destE) 
                                                                                          pure $ Just destE'
                                                                                  else do logM "Client.Destinary" "destinaryInsertPipes" InvalidPacket $ "Provided key does not match the one store for the source : " ++ show sID
                                                                                          pure Nothing



-- | Forges a new destinaryEntry from the sourceID and public key of the source, and a list of pipes.
-- | also configure the timer entry to wait for destinaryTimeOut, and then remove the entry from the map.
-- | if no pipes are provided, start the timer. 
newDestinaryEntry :: SourceID -> PubKey -> [PipeID] -> WeedMonad (Maybe DestinaryEntry)
newDestinaryEntry sID sk pIDs = do uk <- snd . clKeyPair <$> getClient
                                   case genPipeKeys sk uk of
                                        Nothing -> logM "Client.Destinary" "newDestinaryEntry" Error "Unable to generate DH-secret from keys" >> pure Nothing
                                        Just pipeKP -> do logM "Client.Destinary" "newDestinaryEntry" Normal $ "Adding new source : " ++ show sID
                                                          Just <$> ( DestinaryEntry pIDs pipeKP sk <$> timer <*> newComModule sID )
    where timer = do e <- newTimerEntry destinaryTimeOut removeDestinary
                     when (null pIDs) $ startTimer e
                     pure e
          removeDestinary = do logM "Client.Destinary" "newDestinaryEntry" Normal $ "Timeout of source : " ++ show sID
                               stmModify clDestinaries $ M.delete sID


