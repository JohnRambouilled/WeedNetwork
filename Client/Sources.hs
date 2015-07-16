{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies, FlexibleContexts #-}
module Client.Sources where

import Control.Monad.State
import Control.Monad.RWS.Lazy
import Control.Monad.Reader
import Control.Concurrent
import Data.List
import Data.Maybe
import Data.Binary
import qualified Data.Map as M

import Client.Class
import Client.Crypto
import Client.Packet
import Client.Routing
import Client.Pipes
import Client.Communication
import Log

type Sources = MapModule SourceEntry SourceID Request SourceAnswer

type SourceCB = Behaviour Sources Request IO [SourceAnswer]
data SourceAnswer = SourceAnswer { sourceAnsID :: SourceID,
                                   sourceAnswer :: Maybe (DataCB, MVar Pipes)} 

{- TODO Associer un pipe à une route -}
data SourceEntry =  SourceEntry {sourceID :: SourceID,
                                 sourcePipes :: MVar Pipes, 
                                 sourceFreeComID :: MVar [ComID],
                                 sourceCommunication :: MVar Communication}
instance Eq SourceEntry where sE == sE' = sourceID sE == sourceID sE'

instance MapModules SourceEntry SourceID Request SourceAnswer where
        packetKey = do req <- ask
                       if null $ road req then pure Nothing 
                       else return . Just  $ if 0 == roadPosition req then last $ road req
                                                                     else head $ road req
        entryBehaviour (SourceEntry sID pV _ cV) = [ask >>= \r -> return [SourceAnswer sID $ Just (genCommunicationCallback pV cV (roadPosition r) $ roadToRoadID (road r), pV)]]

sendToSource :: SourceEntry -> ComMessage -> IO Bool
sendToSource sE cm = do (pL, pM) <- withMVar (sourcePipes sE) (pure . ((,) <$> pipesList <*> pipesMap))
                        case pL of [] ->  pure False
                                   rID:_ ->  maybe (pure False) (($cm) . writeFun) (M.lookup rID $ pM)



getSourceEntry :: MVar Sources -> SourceID -> IOLog (Maybe SourceEntry)
getSourceEntry sV sID = liftIO $ withMVar sV $ pure . M.lookup sID . keyMap

removeSourceEntry :: MVar Sources -> SourceID -> RawData -> IOLog () 
removeSourceEntry sV sID d = liftIO $ modifyMVar_ sV $ \s -> do let (msE, kM) = M.updateLookupWithKey (pure $ pure Nothing) sID $ keyMap s
                                                                maybe (pure ()) (breakComAndPipes) msE
                                                                pure s{keyMap = kM}
           where breakComAndPipes sE@(SourceEntry _ pV iV cV) = breakCom cV sE >> modifyMVar_ pV breakPipes >> modifyMVar_ iV (pure $ pure [])
                 breakPipes pM = do forM (pipesMap pM) $ ($d) . breakFun
                                    pure (Pipes M.empty [])
                 breakCom :: MVar Communication -> SourceEntry -> IO ()
                 breakCom cV sE = do modifyMVar_ cV $ \s -> do (mapM (sendBrk s sE)  $ M.assocs (keyMap s)) 
                                                               pure $ newMapModule []
		 sendBrk :: Communication -> SourceEntry -> (ComID, ComEntry) -> IO ()
                 sendBrk s sE (cID, ComEntry cCBL) = do --sequence $ map (local $ \_ -> ComExit cID (encode "source removed locally")) cCBL
							runRWST (sequence cCBL) (ComExit cID $ encode "source removed locally") s 
                                                        void . liftIO $ sendToSource sE $ ComExit cID (encode "source removed by peer")
                                                     
                                                       



-- | Standard DestCallback for routing
pipesRoutingCallback ::  PrivKey -> PubKey -> SendFunction -> MVar Sources -> RoutingCB
pipesRoutingCallback uK pK send sV = genCallback sV inFun ((head <$>) . mapM outFun . concat)
    where inFun = pure <$> ask
          outFun (SourceAnswer sID sAnsM) = case sAnsM of
                                                Nothing -> pure $ RoutingAnswer (pure ()) Nothing []
                                                Just (dCB, pV) -> do (onBrk, pL) <- addNewPipe uK pK send pV =<< ask
                                                                     return $ RoutingAnswer (onBrk >> chkSource pV sID) (Just [dCB]) pL 
          chkSource pV sID = do b <- liftIO $ withMVar pV $ pure . null . pipesList
                                if b then removeSourceEntry sV sID (encode ()) 
                                     else pure ()


