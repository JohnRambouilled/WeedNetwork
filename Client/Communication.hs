module Client.Communication where

import           Packets
import           Types

import           Control.Concurrent.STM
import qualified Data.Map               as M

newComModule :: STMIO (TVar ComModule)
newComModule = liftSTM $ newTVar M.empty

-- Les Comid Sont Tirés Random

-- Appelle Le callback correspondant au bon comID (est enregistré dans
-- Pipes par Destinary)
onComMessage :: TVar ComModule -> ComMessage -> STMIO ()
onComMessage comModule comMessage = do comMod <- stmRead comModule
                                       case comID `M.lookup` comMod of
                                            Nothing -> logM $ RawLog $ "[COMMUNICATION] reception d'un com message du comID " ++ show comID ++ " mais il$ est inconnu."
                                            Just comEntry -> case comMessage of
                                                               msg@(ComData _ _)-> runCallback (comCallback comEntry) $ Right msg
                                                               msg@(ComClose _ _) -> do runCallback (comCallback comEntry) $ Left $ ComErrorExit msg
                                                                                        stmWrite comModule $ M.delete comID comMod
    where comID = cmComID comMessage

{- Enregistre le callback dans la map s'il n'existe pas. Renvoie Faux
sinon -}
registerComCallback :: TVar ComModule -> ComID -> Callback ComError ComMessage -> STMIO Bool
registerComCallback comModule comID cb = do comMod <- stmRead comModule
                                            case comID `M.lookup` comMod of
                                              Just comEntry -> pure False
                                              Nothing -> do stmWrite comModule $ M.insert comID (ComEntry cb) comMod
                                                            pure True


