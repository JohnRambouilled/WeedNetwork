module Client.Destinary where

import Types
import Packets
import Client.Crypto
import Client.Communication
import Client.WeedMonad
import Client.Timer

import Control.Lens
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Binary (decodeOrFail)

destinaryTimeOut = 60 :: Time



-- |  Remove a pipe in the destinary pipeMap. If it is the only pipe leading to that source, it start the timeOut timer.
destinaryRemovePipe :: UserID -> PipeID -> WeedMonad ()
destinaryRemovePipe sID pID = do destMod <- stmRead clDestinaries
                                 case sID `M.lookup` destMod of
                                    Nothing -> logM "Client.Destinary" "destinaryRemovePipe" Error "Attempted to remove a pipe from a non-existing source" >> pure ()
                                    Just e -> do stmWrite clDestinaries $ M.insert sID (over destPipes (filter (==pID)) e) destMod
                                                 when (_destPipes e == [pID]) $ startTimer (_destTimer e)



-- | Add the requested pipe in the Destinary map, and create a new DestinaryEntry if necessary.
-- | Returns Nothing if Diffie-Hellman fails (for a new source), or if the key present in the request does not match the one stored for the source.
-- | If the destinary had no other pipes, kills the timeOut timer.
destinaryInsertPipes :: PubKey -> UserID -> [PipeID] -> WeedMonad Bool
destinaryInsertPipes pk sID pIDs = do destMod <- stmRead clDestinaries
                                      case sID `M.lookup` destMod of
                                            Nothing -> do destEM <- newDestinaryEntry sID pk pIDs
                                                          case destEM of
                                                              Nothing -> pure False
                                                              Just destE -> do stmModify clDestinaries $ M.insert sID destE
                                                                               pure True
                                            Just destE -> if pk == _destKey destE then do stmWrite clDestinaries $ M.insert sID (over destPipes (pIDs++) destE) destMod
                                                                                          when (null $ _destPipes destE) $ killTimer (_destTimer destE) 
                                                                                          pure True
                                                                                  else do logM "Client.Destinary" "destinaryOnRequest" InvalidPacket "Request from known source with mismatching key!"
                                                                                          pure False



-- | Forges a new destinaryEntry from the sourceID and public key of the source, and a list of pipes.
-- | also configure the timer entry to wait for destinaryTimeOut, and then remove the entry from the map.
-- | if no pipes are provided, start the timer. 
newDestinaryEntry :: SourceID -> PubKey -> [PipeID] -> WeedMonad (Maybe DestinaryEntry)
newDestinaryEntry sID sk pIDs = do uk <- snd . clKeyPair <$> getClient
                                   case genPipeKeys sk uk of
                                        Nothing -> logM "Client.Destinary" "newDestinaryEntry" Error "Unable to generate DH-secret from keys" >> pure Nothing
                                        Just pipeKP -> Just <$> ( DestinaryEntry pIDs pipeKP sk <$> timer <*> newComModule sID )
    where timer = do e <- newTimerEntry destinaryTimeOut removeDestinary
                     when (null pIDs) $ startTimer e
                     pure e
          removeDestinary = stmModify clDestinaries $ M.delete sID


