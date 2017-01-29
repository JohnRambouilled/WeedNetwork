module Client.Protocol where

import Types
import Packets
import Client.Sender
import Client.WeedMonad

import Data.Binary
import Control.Lens
import Control.Concurrent.STM
import qualified Data.Map as M

-- USELESS!! Already in client.Communication
onComInit :: TVar ComModule -> PipeSender -> ComInit -> WeedMonad ()
onComInit comModule sender comInit = do comMod <- liftSTM $ readTVar comModule 
                                        case comID `M.lookup` view comMap comMod of
                                            Just _ -> do logM "Client.Protocol" "onComInit" InvalidPacket "ComInit with comID already in use"
                                                         close "ComID already in use"
                                            Nothing -> do protoMap <- stmRead clProtocols
                                                          case view ciProtocolID comInit `M.lookup` protoMap of
                                                                Nothing -> do logM "Client.Protocol" "onComInit" InvalidPacket "ComInit for unknown protocol"
                                                                              close "Unknown protocolID"
                                                                Just protoE -> liftSTM $ do comE <- protoE (view comSource comMod) $ view ciPayload comInit
                                                                                            modifyTVar comModule . over comMap $ M.insert comID comE
    where comID = view ciComID comInit
          close s = sender . ComPmessage . ComClose comID $ encode s
