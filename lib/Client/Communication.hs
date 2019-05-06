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

  
maxComID = 100 :: Int

openCommunication :: SourceID -> PipeSender -> ProtocolID -> ComEntry -> RawData -> WeedMonad (Maybe ComID)
openCommunication sID sender pID cE cnt = do
  dMap <- stmRead clDestinaries
  case sID `M.lookup` dMap of
     Nothing -> do logM "Client.Communication" "openCommunication" Error $ "Unable to open communication : source " ++ show sID ++ " does not exist."
                   pure Nothing
     Just dE -> do cID <- ComID <$> getRandomInt (0, maxComID)
                   registerComCallback (view destComModule dE) cID cE 
                   sender [] . ComPinit $ ComInit cID pID cnt 
                   logM "Client.Communication" "openCommunication" Normal $ "New communication " ++ show cID ++ " to source " ++ show sID ++ " has been openned."
                   pure $ Just cID
                                  
                                  
  
  
closeCommunication :: SourceID -> PipeSender -> ComID -> RawData -> WeedMonad ()
closeCommunication sID sender cID d = do deM <- wmLookup sID clDestinaries
                                         case deM of
                                            Nothing -> logM "Client.Communication" "closeCommunication" Error $ "Source " ++ show sID ++ " does not exist"
                                            Just dE -> do removeCommunication (view destComModule dE) cID
                                                          sender [] . ComPmessage $ ComClose cID d
                                                          logM "Client.Communication" "closeCommunication" Normal $ "Communication " ++ show cID ++ " to source " ++ show sID ++ " has been closed."


addProtocol :: ProtocolID -> ProtocolEntry -> WeedMonad Bool
addProtocol pID pE = do pMap <- stmRead clProtocols
                        case pID `M.lookup` pMap of
                          Nothing -> stmModify clProtocols (M.insert pID pE) >> return True
                          Just _ -> do logM "Client.Communication" "addProtocol" Error $ "An entry is already present in the protocol map for protocol : " ++ show pID
                                       return False

  
onComPacket :: TVar ComModule -> PipeID -> ComPacket -> WeedMonad ()
onComPacket m pID (ComPinit ci) =  onComInit m pID ci
onComPacket m pID (ComPmessage cm) = onComMessage m pID cm


-- Crée un nouveau comModule vide, pour la source indiquée
newComModule :: SourceID -> WeedMonad (TVar ComModule)
newComModule = liftSTM . newTVar . ComModule M.empty
            

-- Appelle le callback enregistré pour le comID du message.
-- Supprime le comID en cas de ComExit
onComMessage :: TVar ComModule -> PipeID -> ComMessage -> WeedMonad ()
onComMessage comModule pID comMessage = do comMod <- liftSTM $ readTVar comModule
                                           case comID `M.lookup` view comMap comMod of
                                                Nothing -> logM "Client.Communication" "onComMessage" InvalidPacket $ "reception d'un com message du comID " ++ show comID ++ " mais il est inconnu."
                                                Just comEntry -> do weedIO $ (comCallback comEntry) pID  comMessage
                                                                    logM "Client.Communication" "onComMessage" Normal $ "ComMessage received on communication : " ++ show comID
                                                                    when (isComExit comMessage) $ do removeCommunication comModule comID
                                                                                                     logM "Client.Communication" "onComMessage" Normal $ "ComClose received for communcation " ++ show comID
    where comID = view cmComID comMessage


-- | Pointfree FTW
removeCommunication :: TVar ComModule -> ComID -> WeedMonad ()
removeCommunication comModule = liftSTM . modifyTVar comModule . over comMap . M.delete 

-- | Manage a comInit : if comID is already used, or if protocolID is unknown, close the communication and log and InvalidPacket
-- | Take the PipeID from which the message came, and the ComModule of the destinary
onComInit :: TVar ComModule -> PipeID -> ComInit -> WeedMonad ()
onComInit comModule pipeID comInit = do protoMap <- stmRead clProtocols
                                        case view ciProtocolID comInit `M.lookup` protoMap of
                                            Nothing -> do logM "Client.Communitcation" "onComInit" InvalidPacket "ComInit over an unknown protocol"
                                                          close "unknown ProtocolID"
                                            Just f -> do comMod <- liftSTM $ readTVar comModule
                                                         ce <- liftSTM $ f (view comSource comMod) (view ciPayload comInit)
                                                         b <- registerComCallback comModule comID ce
                                                         if b then logM "Client.Communication" "onComInit" Normal $ "New Communication accepted on comID : " ++ show comID ++ " with the protocol : " ++ show (_ciProtocolID comInit)
                                                         else do logM "Client.Communication" "onComInit" InvalidPacket "ComInit received for an already used comID"
                                                                 close "ComID already in use"
    where close s = do senderM <- genPipeSender pipeID
                       case senderM of Nothing -> logM "Client.Communitcation" "onComInit" Error ("Attempted to close a comInit, but the pipe does not exist... Error was : " ++ s)
                                       Just send -> send [] . ComPmessage . ComClose comID $ encode s
          comID = view ciComID comInit


{- Enregistre le callback dans la map s'il n'existe pas. Renvoie Faux
sinon -}
registerComCallback :: TVar ComModule -> ComID -> ComEntry -> WeedMonad Bool
registerComCallback comModule comID ce = do comMod <- liftSTM $ readTVar comModule
                                            case comID `M.lookup` view comMap comMod of
                                              Just comEntry -> pure False
                                              Nothing -> do liftSTM $ writeTVar comModule (ComModule (M.insert comID ce $ view comMap comMod) $ view comSource comMod)
                                                            pure True



