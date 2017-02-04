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



-- |  Remove a pipe in the destinary pipeMap. If it is the only pipe leading to that source, it start the timeOut timer wich will delete the destinary entry after destinaryTimeOut.
destinaryRemovePipe :: UserID -> PipeID -> WeedMonad ()
destinaryRemovePipe sID pID = do destMod <- stmRead clDestinaries
                                 case sID `M.lookup` destMod of
                                    Nothing -> logM "Client.Destinary" "destinaryRemovePipe" Error "Attempted to remove a pipe from a non-existing source" >> pure ()
                                    Just e -> do stmWrite clDestinaries $ M.insert sID (over destPipes (filter (==pID)) e) destMod
                                                 when (_destPipes e == [pID]) $ startDestinaryTimer (_destTimer e)
    where startDestinaryTimer t  = startTimer t destinaryTimeOut removeDestinary
          removeDestinary = stmModify clDestinaries $ M.delete sID



-- | Add the requested pipe in the Destinary map, and create a new DestinaryEntry if necessary.
-- | Returns Nothing if Diffie-Hellman fails (for a new source), or if the key present in the request does not match the one stored for the source.
-- | If the destinary had no other pipes, kills the timeOut timer.
destinaryOnRequest :: Request -> WeedMonad Bool
destinaryOnRequest req = do destMod <- stmRead clDestinaries
                            case destID `M.lookup` destMod of
                                Nothing -> do destEM <- newDestinaryEntry destID (_reqSourceKey req) [pipeID]
                                              case destEM of
                                                Nothing -> pure False
                                                Just destE -> do stmModify clDestinaries $ M.insert destID destE
                                                                 pure True
                                Just destE -> if _reqSourceKey req == _destKey destE then do stmWrite clDestinaries $ M.insert destID (over destPipes (pipeID:) destE) destMod
                                                                                             when (null $ _destPipes destE) $ killTimer (_destTimer destE) 
                                                                                             pure True
                                                                                     else do logM "Client.Destinary" "destinaryOnRequest" InvalidPacket "Request from known source with mismatching key!"
                                                                                             pure False
    where destID = head $ _reqRoad req
          pipeID = _reqPipeID req



-- | Forges a new destinaryEntry from the sourceID and public key of the source, and a list of pipes.
newDestinaryEntry :: SourceID -> PubKey -> [PipeID] -> WeedMonad (Maybe DestinaryEntry)
newDestinaryEntry sID sk pIDs = do uk <- snd . clKeyPair <$> getClient
                                   case genPipeKeys sk uk of
                                        Nothing -> logM "Client.Destinary" "newDestinaryEntry" Error "Unable to generate DH-secret from keys" >> pure Nothing
                                        Just pipeKP -> Just <$> ( DestinaryEntry pIDs pipeKP sk <$> createTimerEntry <*> newComModule sID )


