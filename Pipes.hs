{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, MultiParamTypeClasses #-}
module Pipes where

import Crypto
--import Routing 
import Class
import PipePackets
import Timer


import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M

pipeTimeOut = 10 :: Time


data NewPipe = NewPipe {npRoad :: Road,
                        npSource :: SourceID,
                        npPubKey :: PubKey,
                        npSender :: Handler PipeMessage,
                        npPipeID :: PipeID,
                        npTime :: Time,
                        npContent :: RawData,
                        npMessageEvent :: EventC PipeMessage}


type SourceMap = M.Map SourceID SourceEntry
type SourceMapH = M.Map SourceID (SourceEntry, Handler NewPipe)

data SourceEntry = SourceEntry {seReceiver :: EventC Payload,
                                seSender :: Handler Payload,
                                sePipeMap :: BehaviorC PipeMap}

data PipeEntry = PipeEntry {pePipe :: EventC PipeMessage,
                            peSender :: Handler PipeMessage}

type PipeMap = M.Map PipeID PipeEntry

buildSourceMap :: Event NewPipe -> MomentIO (BehaviorC SourceMap)
buildSourceMap newPipeE = do sMap <- newBehaviorMod M.empty
                             execute $ applyMod onNewPipe sMap newPipeE
                             pure $ fmap fst <$> bmBhvC sMap
    where onNewPipe ::  Modifier SourceMapH -> SourceMapH -> NewPipe -> MomentIO ()
          onNewPipe mod map np = case sID `M.lookup` map of
                                       Just (_,h) -> liftIO $ h np
                                       Nothing -> do sE <- newSourceEntry
                                                     liftIO . snd sE $ np
                                                     liftIO . mod $ M.insert sID sE
                    where sID = npSource np
                    

sendToSource :: SourceMap -> (SourceID, Payload) -> IO ()
sendToSource sM (sID, p) = case sID `M.lookup` sM of
                                Nothing -> pure ()
                                Just se -> seSender se $ p


--listSourcePipes :: BehaviorC SourceMap -> MomentIO (Event [SourceID, [PipeID]])
--listSourcePipes sm = execute $ dumpMap <$> bcChanges sm
--    where dumpMap :: SourceMap -> MomentIO ([S



newSourceEntry :: MomentIO (SourceEntry, Handler NewPipe)
newSourceEntry = do (sendE, sendH) <- newEvent
                    (newPipeE, newPipeH) <- newEvent
                    (sourceE,pipeMap) <- buildSource newPipeE sendE
                    pure (SourceEntry sourceE sendH pipeMap, newPipeH)

-- | Manage a single source : take the event of opening pipes, and the event of message to send to se source. Return the messages received from the source (with the close Event of the source)
buildSource :: Event NewPipe -> Event Payload -> MomentIO (EventC Payload, BehaviorC PipeMap)
buildSource newPipeE toSendE = do (closeSourceE, closeSourceH) <- newEvent
                                  let newPipeEntryE = makePipeEntry <$> newPipeE
                                  pipeMap <- buildEventCMapWith newPipeEntryE 
                                  receiveE <- mergeEvents $ (pePipe <$>) <$> bmChanges pipeMap
                                  let (closeE, msgE) = split (receiveE :: Event PipeMessage)
                                  buildTimeOut pipeTimeOut (fst <$> newPipeEntryE) never 
                                  reactimate $ applyMod closePipe pipeMap closeE
                                  reactimate $ apply (sendToSource <$> bmLastValue pipeMap) toSendE
                                  reactimate $ applyMod closeSource pipeMap closeSourceE
                                  pure (EventC (snd <$> msgE) closeSourceH closeSourceE,
                                        bmBhvC pipeMap)
    where sendToSource :: PipeMap -> Handler Payload
          sendToSource pMap = sendOnPipe . head $ M.assocs pMap
          sendOnPipe :: (PipeID, PipeEntry) -> Handler Payload
          sendOnPipe (pID,pE) d = peSender pE $ Right (pID,d)
          makePipeEntry :: NewPipe -> ((PipeID, EventC PipeMessage), PipeEntry)
          makePipeEntry np = ((npPipeID np, npMessageEvent np), PipeEntry (npMessageEvent np) $ npSender np)
          closePipe :: Modifier PipeMap -> PipeMap -> (PipeID, Payload) -> IO ()
          closePipe _ map (pID,_) = case pID `M.lookup` map of
                                        Just e -> ceClose (pePipe e) $ ()
                                        Nothing -> pure ()
          closeSource :: Modifier PipeMap -> PipeMap -> () -> IO ()
          closeSource mod map _ = do mapM_ closeP $ M.elems map
                                     mod $ pure M.empty
                where closeP e = ceClose (pePipe e) $ ()

