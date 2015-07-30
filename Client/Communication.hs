{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies, FlexibleContexts #-}
module Client.Communication where

import Control.Monad.State hiding (put, get)
import qualified Control.Monad.State  as S (get)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Concurrent
import Data.Binary 
import Data.Maybe
import Data.List
import qualified Data.Map as M

import Client.Class
import Client.Crypto
import Client.Packet
import Client.Routing
import Client.Pipes
import Log


type Communication = MapModule ComEntry ComID ComMessage () 
type ComCB = Behaviour Communication ComMessage IO [()]
type ComS a = Behaviour Communication ComMessage IO a
--type ComT = StateT Communication

data ComEntry = ComEntry {comCallback :: [ComCB]}

instance MapModules ComEntry ComID ComMessage () where
        packetKey = do cm <- ask
                       keepLog CommunicationLog Normal $ "receivedComMess : " ++ show cm
                       S.get >>= keepLog CommunicationLog Normal . show
                       return . Just $ comID cm
        entryBehaviour = comCallback



insertComCallback :: MVar Communication -> ComID -> ComCB -> IOLog ()
insertComCallback cV cID clbk = liftIO $ modifyMVar_ cV $ \s -> pure s{keyMap = M.insert cID (ComEntry [clbk]) $ keyMap s}

closeComIO :: MVar [ComID] -> MVar Communication -> ComID -> IOLog ()
closeComIO fV cV cID = runSWMVar cV $ closeCom fV cID
                        

closeCom :: (MonadState Communication m, LogIO m) => MVar [ComID] -> ComID -> m ()
closeCom fV cID =  do keepLog CommunicationLog Important $ "Closing communication : " ++ show cID
                      modify $ \s -> s{keyMap = M.delete cID $ keyMap s}
                      liftIO . modifyMVar_ fV $ pure .(++ [cID])



defaultComCallback :: Modules a p r => MVar a 
                                   -> (ComMessage -> Maybe p)
                                   -> (ComID -> p -> r -> ComS (Maybe ComCB))
                                   -> ComCB
defaultComCallback mv iFun oFun = do p <- ask
                                     case iFun p of
                                        Nothing -> return []
                                        Just q -> do keepLog CommunicationLog Important $ "calling comCB"
                                                     genCallback mv (pure . pure $ q) (outFun q)
    where outFun q rL = do p <- ask
                           comCBL <- (catMaybes <$>) $ mapM (oFun (comID p) q) rL
                           insertMapBehaviour (comID p) (ComEntry $ comCBL)
                           pure []






genCommunicationCallback :: MVar Pipes -> MVar Communication -> Number -> RoadID -> DataCB
genCommunicationCallback pV cV n0 rID = DataCB (ask >>= hFun) (ask >>= clbk)
    where hFun :: Packet -> HashFunction (Either ComMessage Packet)
          hFun (DataPacket kH _ (DataContent cnt)) = case decodeMaybe cnt of
                                        Nothing -> return Nothing
                                        Just (PipeData n b cm) -> if n == n0 then do keepLog CommunicationLog Normal $ "received pipeData on pipe : " ++ show kH 
                                                                                     case decodeMaybe cm of Nothing -> do keepLog CommunicationLog Error "fail to decode" 
                                                                                                                          pure Nothing 
                                                                                                            Just x -> do keepLog CommunicationLog Normal $ show x 
                                                                                                                         return $ Just (pipeMessageHash kH b cm, Left x)
                                                                             else pure Nothing
                                        Just (PipeExit n _ _) -> if n == n0 then do unregisterKeyEntry kH
                                                                                    removePipe pV rID
                                                                                    return  Nothing
                                                                            else pure Nothing
          hFun p@(Introduce kH _ _ (IntroContent cnt)) = case decodeMaybe cnt :: Maybe Request of
                                                             Nothing -> pure Nothing
                                                             Just req -> if n0 == 0 && roadPosition req == n0 then
                                                                               return $ Just (reqSourceHash kH req, 
                                                                                         Right p{introContent = IntroContent $ encode req{roadPosition = roadPosition req + 1}})
                                                                         else pure Nothing 
          clbk :: Either ComMessage Packet -> CryptoCB (Either ComMessage Packet) [Packet]
          clbk (Left cm) = do tell . snd =<< (liftIO $ (runModule cV cm :: IO ([()],Log)) )
                              return []
          clbk (Right pkt) = pure [pkt]






