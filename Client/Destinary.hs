module Client.Destinary where

import Types
import Packets
import Client.Crypto
import Client.Communication
import Client.WeedMonad

import Control.Lens
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Binary (decodeOrFail)


-- | Add the requested pipe in the Destinary map, and create a new DestinaryEntry if necessary.
-- | Returns Nothing if Diffie-Hellman fails (for a new source), or if the key present in the request does not match the one stored for the source.
destinaryOnRequest :: Request -> WeedMonad (Maybe (TVar ComModule))
destinaryOnRequest req = do destMod <- stmRead clDestinaries
                            case destID `M.lookup` destMod of
                                Nothing -> do destEM <- newDestinaryEntry destID (_reqSourceKey req) [pipeID]
                                              case destEM of
                                                Nothing -> pure Nothing
                                                Just destE -> do stmModify clDestinaries $ M.insert destID destE
                                                                 pure . Just $ view destComModule destE
                                Just destE -> if _reqSourceKey req == _destKey destE then do stmWrite clDestinaries $ M.insert destID (over destPipes (pipeID:) destE) destMod
                                                                                             pure . Just $ _destComModule destE
                                                                                     else do logM "Client.Destinary" "destinaryOnRequest" InvalidPacket "Request from known source with mismatching key!"
                                                                                             pure Nothing
    where destID = head $ _reqRoad req
          pipeID = _reqPipeID req



newDestinaryEntry :: SourceID -> PubKey -> [PipeID] -> WeedMonad (Maybe DestinaryEntry)
newDestinaryEntry sID sk pIDs = do uk <- snd . clKeyPair <$> getClient
                                   case genPipeKeys sk uk of
                                        Nothing -> logM "Client.Destinary" "newDestinaryEntry" Error "Unable to generate DH-secret from keys" >> pure Nothing
                                        Just pipeKP -> Just . DestinaryEntry pIDs pipeKP sk <$> newComModule sID


