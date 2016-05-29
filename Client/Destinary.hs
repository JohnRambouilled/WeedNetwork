
module Client.Destinary where

import Types
import Packets
import Client.Crypto
import Client.Protocol
import Client.Pipes

import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Binary (decodeOrFail)

destinaryOnRequest :: TVar PipesModule -> TVar DestinaryModule -> TVar ProtocolModule -> Request -> STMIO ()
destinaryOnRequest pipesModule destModule protoModule req = do
  destMod <- stmRead destModule
  case destID `M.lookup` destMod of
               Nothing -> do destE <- newDestinaryEntry (reqDHPubKey req) (reqPipeID req) (reqPosition req)
                             stmModify destModule $ M.insert destID destE 
                             addPipe destE
               Just destE -> if reqDHPubKey req == destinaryDHPubKey destE then addPipe destE
                                                                           else pure ()
                                                   
    where destID = reqRoad req !! 0
          callback :: DestinaryEntry -> Either PipeError PipeData -> STMIO ()
          callback _ (Left (PipeClosedError _)) = stmModify destModule $ M.adjust removePipe destID
          callback destEntry (Right pkt) = do case decodeOrFail (pipeDPayload pkt) of
                                                Left (_,_,e) -> logM $ RawLog "[Destinary] impossible de lire le contenu du message"
                                                Right (_,_,ComPinit comI) -> onComInit protoModule (destinaryComModule destEntry) (destinarySender destEntry $ DefaultSender) comI
          pID = reqPipeID req
          removePipe :: DestinaryEntry -> DestinaryEntry
          removePipe entry = entry{destinaryPipes = M.delete pID $ destinaryPipes entry}
          addPipe :: DestinaryEntry -> STMIO ()
          addPipe entry = do registerPipe pipesModule req $ Callback (callback entry)
                             stmModify destModule $ M.insert destID entry{destinaryPipes = M.insert pID (genPipeSender req) $ destinaryPipes entry}   

newDestinaryEntry :: DHPubKey -> PipeID -> Number -> STMIO DestinaryEntry
newDestinaryEntry yourDH pID pos = do myDH <- lift $ asks ( snd . stmDHKeys)
                                      case decryptKeyPair yourDH myDH of
                                        Just keyPair -> do comModule <- liftSTM $ newTVar M.empty
                                                           pure $ DestinaryEntry M.empty yourDH genDestinarySender comModule 
                                        Nothing -> error "[Destinary] Impossible de déchiffrer le secret"

genDestinarySender opt _ = pure ()

genPipeSender ::Request -> RawData -> STMIO ()
genPipeSender req _ = pure ()



