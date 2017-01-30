module Client.Communication where

import Packets
import Types
import Client.WeedMonad
import Client.Sender

import Data.Binary
import Control.Lens
import Control.Monad
import Control.Concurrent.STM
import qualified Data.Map as M


onComPacket :: TVar ComModule -> PipeSender -> ComPacket -> WeedMonad ()
onComPacket m s (ComPinit ci) =  onComInit m s ci
onComPacket m _ (ComPmessage cm) = onComMessage m cm


-- Crée un nouveau comModule vide, pour la source indiquée
newComModule :: SourceID -> WeedMonad (TVar ComModule)
newComModule = liftSTM . newTVar . ComModule M.empty
            

-- Appelle le callback enregistré pour le comID du message.
--
onComMessage :: TVar ComModule -> ComMessage -> WeedMonad ()
onComMessage comModule comMessage = do comMod <- liftSTM $ readTVar comModule
                                       case comID `M.lookup` view comMap comMod of
                                            Nothing -> logM "Client.Communication" "onComMessage" InvalidPacket $ "reception d'un com message du comID " ++ show comID ++ " mais il$ est inconnu."
                                            Just comEntry -> do weedIO $ (comCallback comEntry)  comMessage
                                                                when (isComExit comMessage) $ liftSTM (writeTVar comModule $ over comMap (M.delete comID) comMod)
                                                                                                --liftSTM (writeTVar comModule $ ComModule (M.delete comID $ view comMap comMod) (view comSource comMod))
    where comID = view cmComID comMessage

{- Enregistre le callback dans la map s'il n'existe pas. Renvoie Faux
sinon -}
registerComCallback :: TVar ComModule -> ComID -> ComEntry -> WeedMonad Bool
registerComCallback comModule comID ce = do comMod <- liftSTM $ readTVar comModule
                                            case comID `M.lookup` view comMap comMod of
                                              Just comEntry -> pure False
                                              Nothing -> do liftSTM $ writeTVar comModule (ComModule (M.insert comID ce $ view comMap comMod) $ view comSource comMod)
                                                            pure True

-- | Manage a comInit : if comID is already used, or if protocolID is unknown, close the communication and log and InvalidPacket
onComInit :: TVar ComModule -> PipeSender -> ComInit -> WeedMonad ()
onComInit comModule sender comInit = do protoMap <- stmRead clProtocols
                                        case view ciProtocolID comInit `M.lookup` protoMap of
                                            Nothing -> do logM "Client.Communitcation" "onComInit" InvalidPacket "ComInit over an unknown protocol"
                                                          close "unknown ProtocolID"
                                            Just f -> do comMod <- liftSTM $ readTVar comModule
                                                         ce <- liftSTM $ f (view comSource comMod) (view ciPayload comInit)
                                                         b <- registerComCallback comModule comID ce
                                                         if b then logM "Client.Communication" "onComInit" Normal $ "New Communication accepted on comID : " ++ show comID ++ " with the protocol : " ++ show (_ciProtocolID comInit)
                                                         else do logM "Client.Communication" "onComInit" InvalidPacket "ComInit received for an already used comID"
                                                                 close "ComID already in use"
    where close s = sender . ComPmessage . ComClose comID $ encode s
          comID = view ciComID comInit



