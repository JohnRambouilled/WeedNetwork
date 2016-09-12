module Client.Protocol where

import Types
import Packets

import Data.Binary
import Control.Concurrent.STM
import qualified Data.Map as M

onComInit :: TVar ProtocolModule ->  TVar ComModule -> PipeSender -> ComInit -> STMIO ()
onComInit protoModule comModule sender comInit = do (protoMod, comMod) <- (,) <$> stmRead protoModule <*> stmRead comModule
                                                    case ciComID comInit `M.lookup` comMod of
                                                        Just _ -> do close "ComID déja utilisé, fermeture de la communication" 
                                                                     stmModify comModule $ M.delete (ciComID comInit) 
                                                        Nothing -> case decodeOrFail (ciPayload comInit) of
                                                                      Left (_,_,e) -> do logM $ RawLog "Impossible de déchiffrer le ProtocolID"
                                                                                         close "Impossible de déchiffrer le ProtocolID"
                                                                      Right (payload, _, protoID) -> case protoID `M.lookup` protoMod of
                                                                                                        Nothing -> close "Protocol inconnu"
                                                                                                        Just protoE -> protoE payload >>= addCom
                where comClose :: String -> ComMessage
                      comClose s = ComClose (ciComID comInit) (encode s)
                      close s = sender $ encode (ComPmessage (comClose s))
                      addCom :: Callback ComError ComMessage -> STMIO ()
                      addCom clbk = stmModify comModule $ M.insert (ciComID comInit) (ComEntry clbk)







