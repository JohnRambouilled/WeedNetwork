{-# LANGUAGE DeriveGeneric #-}
module Pipes where

import Crypto
import Routing 
import Class


import FRP.Sodium
import FRP.Sodium.Internal hiding (Event)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Data.Binary
import Control.Monad
import qualified Data.Map as M

type UserID = KeyHash


type PipesSender = Handler PipeMessage
type PipesSenderMap = M.Map PipeID PipesSender
type PipesSenderManager = M.Map UserID (Behavior PipesSenderMap)
type PipesSenderManagerBhv = Behavior PipesSenderManager

type PipeCloser = Handler (SourceID, PipeID)

data Pipes = Pipes {pipesManager :: PipesManagerBhv,
                    pipesMessagesOut :: Event PipePacket,
                    pipesClosePipe :: PipeCloser,
                    pipesRemoveSource :: Handler SourceID} 




buildPipes :: Modifier RoutingMap -> Event NewPipe -> Reactive Pipes
buildPipes routMapMod npE = do (closeE, closeH) <- newEvent'
                               (remSE, remSH) <- newEvent'
                               ((recMapB, recMapM), newSourceE) <- buildRecipientMap npE
                               (pipeManB, sendE) <- buildPipeManager closeH newSourceE
                               listenTrans (snapshot closePipe closeE $ fst pipeManB) id
                               listenTrans remSE $ removeSource closeH recMapM pipeManB
                               pure $ Pipes pipeManB sendE closeH remSH
    where closePipe (sID, pID) pM = case M.lookup sID pM of
                                        Just (_, (_, mod)) -> do mod $ M.delete pID
                                                                 routMapMod $ M.delete pID
                                        Nothing -> pure ()
          removeSource :: PipeCloser -> Modifier RecipientMap -> BhvTpl PipesManager -> SourceID -> Reactive ()
          removeSource closeH recM (manB, manM) sID = do man <- sample manB
                                                         case M.lookup sID man of
                                                            Nothing -> pure ()
                                                            Just (_, (pipeB, _)) -> do 
                                                                   pIDs <- M.keys <$> sample pipeB
                                                                   forM pIDs $ \pID -> fire closeH $ (sID, pID)
                                                                   recM $ M.delete sID
                                                                   manM $ M.delete sID




type PipeManagerEntry =  (Event (Reactive ()), BhvTpl PipesMap )

type PipesManager = M.Map SourceID PipeManagerEntry
type PipesManagerBhv = BhvTpl PipesManager
type NewSourceEvent = Event (SourceID, Event NewPipe)


buildPipeManager :: PipeCloser -> NewSourceEvent -> Reactive (PipesManagerBhv, Event PipePacket)
buildPipeManager closeH newSE = do (sendE, sendH) <- newEvent'
                                   pmBhvTpl@(pipeManB, pipeManH) <- newBhvTpl M.empty 
                                   listenTrans (newSourcePipeMap sendH) $ \(sID, pmE) -> pipeManH $ M.insert sID pmE
                                   listenTrans (allEvents pipeManB) id
                                   pure (pmBhvTpl, sendE)
        where newSourcePipeMap :: Handler PipePacket -> Event (SourceID, PipeManagerEntry )
              newSourcePipeMap sendH = execute $ buildPipeMap closeH sendH <$> newSE
                                           


type DataManager = EventMap SourceID RawData
buildDataManager :: Behaviour PipesManager -> Behaviour DataManager
buildDataManager pManaB = fmap (fmap (snd . fromRight) . filterE isRight . allEvents . snd) <$> pManaB



{-| Converts the map of physical neighbors into a map of recipients |-}
type RecipientMap = EventEntryMap SourceID NewPipe
buildRecipientMap :: Event NewPipe -> Reactive (BhvTpl RecipientMap, NewSourceEvent) 
buildRecipientMap npE = do (rMapB, rMapH) <- newBhvTpl M.empty
                           (newSE, newSH) <- newEvent
                           listenTrans (snapshot (f newSH rMapH) npE rMapB) id
                           pure ((rMapB, rMapH), newSE)
  where f :: ((SourceID, Event NewPipe) -> Reactive ()) -> Modifier RecipientMap -> (Request, Event PipeMessage) -> RecipientMap -> Reactive ()
        f nsH h (req, stream) rM = case sID `M.lookup` rM of
                                     Just eE -> fire (eFire eE) $ (req, stream)
                                     Nothing -> do eE <- newEventEntry (pure True)
                                                   h $ M.insert sID eE 
                                                   nsH (sID, eEvent eE)
                                                   fire (eFire eE) $ (req, stream)
                where sID = last $ reqRoad req


{-| Manages the pipes for a given recipient  : add new pipes on request, forge the sending function, and delete pipes from the routing map on closes.|-}
type PipesMap = M.Map PipeID (Event PipeMessage, PipesSender) --EventMap PipeID PipeMessage
buildPipeMap :: PipeCloser -> Handler PipePacket -> (SourceID, Event NewPipe) -> Reactive (SourceID, PipeManagerEntry) 
buildPipeMap closeH sendH (sID, npE) = do pmBhv@(pMapB,order) <- newBhvTpl M.empty
                                          pure (sID, (toListen order pMapB, pmBhv ) )
    where onNewPipe :: (Request, Event PipeMessage) -> PipesMap -> Maybe (PipesMap -> PipesMap)
          onNewPipe (req,pE) pMap = case reqPipeID req `M.lookup` pMap of
                                            Just _ -> Nothing
                                            Nothing -> Just $ M.insert (reqPipeID req) (pE, makeSender req)
          makeSender req = Handler (\_ -> pure ())  --TODO 
          toListen :: Modifier PipesMap -> Behaviour PipesMap -> Event (Reactive ())
          toListen order pMapB = merge newPipeOrder decoOrder
                where newPipeOrder = order <$> (filterJust $ snapshot onNewPipe npE pMapB)
                      decoOrder = (\pID -> fire closeH $ (sID, pID)) <$> decoE pMapB
                      decoE :: Behaviour PipesMap -> Event PipeID
                      decoE pMapB = fst . fromLeft <$> (filterE isLeft $ allEvents pMapB)


