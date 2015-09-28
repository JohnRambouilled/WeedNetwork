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
type PipeCloser = Handler (SourceID, PipeID)
type NewSourceEvent = Event (SourceID, Event NewPipe)

type PipesMap = M.Map PipeID (Event PipeMessage, PipesSender)

data PipeManagerEntry =  PipeManagerEntry { pmeFireNP :: Handler NewPipe,
                                            pmeToListen :: Event (Reactive ()),
                                            pmePipeMapBhv :: BhvTpl PipesMap }
type PipesManager = M.Map SourceID PipeManagerEntry
type PipesManagerBhv = BhvTpl PipesManager

data Pipes = Pipes {pipesManager :: PipesManagerBhv,
                    pipeNewSourceEvent :: NewSourceEvent,
                    pipesMessagesOut :: Event PipePacket,
                    pipeNewPipe :: Handler NewPipe,
                    pipesClosePipe :: PipeCloser,
                    pipesRemoveSource :: Handler SourceID} 

buildPipes :: Reactive Pipes
buildPipes = do (closeE, closeH) <- newEvent'
                (npE, npH) <- newEvent'
                (remSE, remSH) <- newEvent'
                (pipeManB, sendE, newSourceE) <- buildPipeManager closeH npE
                listenTrans (snapshot closePipe closeE $ fst pipeManB) id
                listenTrans remSE $ removeSource closeH pipeManB
                pure $ Pipes pipeManB newSourceE sendE npH closeH remSH
    where closePipe (sID, pID) pM = maybe (pure ()) (\pme -> snd (pmePipeMapBhv pme) $ M.delete pID) $ M.lookup sID pM
          removeSource :: PipeCloser -> BhvTpl PipesManager -> SourceID -> Reactive ()
          removeSource closeH (manB, manM) sID = do man <- sample manB
                                                    case M.lookup sID man of
                                                            Nothing -> pure ()
                                                            Just pme -> do pIDs <- M.keys <$> sample (fst $ pmePipeMapBhv pme)
                                                                           forM pIDs $ \pID -> fire closeH $ (sID, pID)
                                                                           manM $ M.delete sID


type DataManager = EventMap SourceID RawData
buildDataManager :: Behaviour PipesManager -> Behaviour DataManager
buildDataManager pManaB = fmap (fmap (snd . fromRight) . filterE isRight . allEvents . pmePipeMapBhv) <$> pManaB


{-| Converts the map of physical neighbors into a map of recipients |-}
buildPipeManager :: PipeCloser -> Event NewPipe -> Reactive (PipesManagerBhv, Event PipePacket, NewSourceEvent)
buildPipeManager closeH npE = do (manB, manH) <- newBhvTpl M.empty
                                 (nsE, nsH) <- newEvent'
                                 (sendE, sendH) <- newEvent'
                                 listenTrans (snapshot (f nsH sendH manH) npE manB) id
                                 pure ((manB, manH), sendE, nsE)
  where f :: Handler (SourceID, Event NewPipe) -> Handler PipePacket -> Modifier PipesManager -> NewPipe -> PipesManager -> Reactive ()
        f nsH sendH mod np rM = case sID `M.lookup` rM of
                                     Just pme -> fire (pmeFireNP pme) $ np
                                     Nothing -> do (e, h) <- newEvent'
                                                   (events, pMapB) <- buildPipeMap closeH sendH sID e
                                                   mod $ M.insert sID $ PipeManagerEntry h events pMapB
                                                   fire nsH $ (sID, e)
                                                   fire h $ np
                where sID = npSource np

{-| Manages the pipes for a given recipient  : add new pipes on request, forge the sending function, and delete pipes from the routing map on closes.|-}
buildPipeMap :: PipeCloser -> Handler PipePacket -> SourceID -> Event NewPipe -> Reactive (Event (Reactive ()), BhvTpl PipesMap) 
buildPipeMap closeH sendH sID npE = do pmBhv@(pMapB,order) <- newBhvTpl M.empty
                                       pure (toListen order pMapB, pmBhv ) 
    where onNewPipe :: NewPipe -> PipesMap -> Maybe (PipesMap -> PipesMap)
          onNewPipe np pMap = case npPipeID np `M.lookup` pMap of
                                            Just _ -> Nothing
                                            Nothing -> Just $ M.insert (npPipeID np) (npMessageEvent np, Handler $ fire sendH . npSender np)
          toListen :: Modifier PipesMap -> Behaviour PipesMap -> Event (Reactive ())
          toListen order pMapB = merge newPipeOrder decoOrder
                where newPipeOrder = order <$> (filterJust $ snapshot onNewPipe npE pMapB)
                      decoOrder = (\pID -> fire closeH $ (sID, pID)) <$> decoE pMapB
                      decoE :: Behaviour PipesMap -> Event PipeID
                      decoE pMapB = fst . fromLeft <$> (filterE isLeft $ allEvents pMapB)


