module Client.Destinary where

import Types
import Packets
import Client.Crypto
import Client.Protocol

import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Map as M

destinaryOnRequest :: TVar DestinaryModule -> TVar ProtocolModule -> Request -> STMIO ()
destinaryOnRequest destModule protoModule req = do destMod <- stmRead destModule
                                                   destE <- case destID `M.lookup` destMod of
                                                                Nothing -> do destE <- newDestinaryEntry (reqDHPubKey req) (reqPipeID req) (reqPosition req)
                                                                              stmModify destModule $ M.insert destID destE 
                                                                              pure destE
                                                                Just destE -> pure destE
                                                   pure ()
                                                   
    where destID = reqRoad req !! 0
          callback :: DestEntry -> PipePacket -> STMIO ()
          callback destEntry (PipeClose pipeID _ _ _ _) = stmModify destModule $ M.insert destID destEntry{destinaryPipes = delete pipeID $ destinaryPipes destEntry}
          callback destEntry pkt = do case decodeOrFail (pipePayload pkt) of
                                        Left (_,_,e) -> logM "[Destinary] impossible de lire le contenu du message"
                                        Right (_,_,ComPinit comI) -> onComInit destEntry protoModule

newDestinaryEntry :: DHPubKey -> PipeID -> Number -> STMIO DestinaryEntry
newDestinaryEntry yourDH pID pos = do myDH <- lift $ asks ( snd . stmDHKeys)
                                      case decryptKeyPair yourDH myDH of
                                        Just keyPair -> do comModule <- liftSTM $ newTVar M.empty
                                                           pure $ DestinaryEntry [] keyPair comModule 
                                        Nothing -> error "[Destinary] Impossible de déchiffrer le secret"


send :: 


