module Client.Communication where

import Packets
import Types

import Control.Concurrent.STM
import qualified Data.Map as M

newComModule :: WeedMonad (TVar ComMap)
newComModule = liftSTM $ newTVar M.empty

-- Les Comid Sont Tirés Random

-- Appelle Le callback correspondant au bon comID (est enregistré dans
-- Pipes par Destinary)
onComMessage :: TVar ComMap -> ComMessage -> WeedMonad ()
onComMessage comModule comMessage = do comMod <- liftSTM $ readTVar comModule
                                    case comID `M.lookup` comMod of
                                            Nothing -> logM $ "[Client.Communication : onComMessage] reception d'un com message du comID " ++ show comID ++ " mais il$ est inconnu."
                                            Just comEntry -> case comMessage of
                                                               msg@(ComData _ _)-> weedIO $ (comCallback comEntry)  msg
                                                               msg@(ComClose _ _) -> do weedIO $ (comCallback comEntry)  msg
                                                                                        liftSTM $ writeTVar comModule  (M.delete comID comMod)
    where comID = cmComID comMessage

{- Enregistre le callback dans la map s'il n'existe pas. Renvoie Faux
sinon -}
registerComCallback :: TVar ComMap -> ComID -> (ComMessage -> IO ()) -> STMIO Bool
registerComCallback comModule comID cb = do comMod <- liftSTM . readTVar comModule
                                            case comID `M.lookup` comMod of
                                              Just comEntry -> pure False
                                              Nothing -> do liftSTM $ writeTVar comModule (M.insert comID (ComEntry cb) comMod)
                                                            pure True


