module Client.Communication where

import Packets
import Types
import Client.WeedMonad

import Control.Monad
import Control.Concurrent.STM
import qualified Data.Map as M

newComModule :: SourceID -> WeedMonad (TVar ComModule)
newComModule = liftSTM . newTVar . ComModule M.empty
            

-- Les Comid Sont Tirés Random

-- Appelle Le callback correspondant au bon comID (est enregistré dans
-- Pipes par Destinary)
onComMessage :: TVar ComModule -> ComMessage -> WeedMonad ()
onComMessage comModule comMessage = do comMod <- liftSTM $ readTVar comModule
                                       case comID `M.lookup` comMap comMod of
                                            Nothing -> logM $ "[Client.Communication : onComMessage] reception d'un com message du comID " ++ show comID ++ " mais il$ est inconnu."
                                            Just comEntry -> do weedIO $ (comCallback comEntry)  comMessage
                                                                when (isComExit comMessage) $ liftSTM (writeTVar comModule $ ComModule (M.delete comID $ comMap comMod) (comSource comMod))
    where comID = cmComID comMessage

{- Enregistre le callback dans la map s'il n'existe pas. Renvoie Faux
sinon -}
registerComCallback :: TVar ComModule -> ComID -> ComEntry -> WeedMonad Bool
registerComCallback comModule comID ce = do comMod <- liftSTM $ readTVar comModule
                                            case comID `M.lookup` comMap comMod of
                                              Just comEntry -> pure False
                                              Nothing -> do liftSTM $ writeTVar comModule (ComModule (M.insert comID ce $ comMap comMod) $ comSource comMod)
                                                            pure True

onComInit :: TVar ComModule -> ComInit -> WeedMonad ()
onComInit comModule comInit = do protoMap <- stmRead clProtocols
                                 case ciProtocolID comInit `M.lookup` protoMap of
                                    Nothing -> logM "[Client.Communitcation : onComInit] ComInit over an unknown protocol"
                                    Just f -> do comMod <- liftSTM $ readTVar comModule
                                                 ce <- liftSTM $ f (comSource comMod) (ciPayload comInit)
                                                 b <- registerComCallback comModule (ciComID comInit) ce
                                                 if b then pure () else logM "[Client.Communication : onComInit] ComInit received for an already used comID"



