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

type PipesMap = EventMap PipeID PipeMessage
type PipesManager = EventManager UserID PipeID PipeMessage
type PipesManagerBhv = Behavior PipesManager

type PipesSender = Handler PipeMessage
type PipesSenderMap = M.Map PipeID PipesSender
type PipesSenderManager = M.Map UserID (Behavior PipesSenderMap)
type PipesSenderManagerBhv = Behavior PipesSenderManager

data Pipes = Pipes {pipesManager :: PipesManager,
                    pipesSenders :: PipesSenderManager,
                    pipesCloser :: Handler (SourceID, PipeID)}

{-| Converts the map of physical neighbors into a map of recipients |-}
type RecipientMap = EventEntryMap SourceID NewPipe
buildRecipientMap :: Event NewPipe -> Reactive (BhvTpl RecipientMap)
buildRecipientMap npE = do (rMapB, rMapH) <- newBhvTpl M.empty
                           listenTrans (snapshot (f rMapH) npE rMapB) id
                           pure (rMapB, rMapH)
  where f :: Modifier RecipientMap -> (Request, Event PipeMessage) -> RecipientMap -> Reactive ()
        f h (req, stream) rM = case sID `M.lookup` rM of
                                 Just eE -> fire (eFire eE) $ (req, stream)
                                 Nothing -> do eE <- newEventEntry (pure True)
                                               h $ M.insert sID eE 
                                               fire (eFire eE) $ (req, stream)
                where sID = last $ reqRoad req


{-| Manages the pipes for a given recipient |-}
type PipeMap = EventMap PipeID PipeMessage
buildPipeMap :: Event NewPipe -> Reactive (BhvTpl PipeMap)
buildPipeMap npE = do (pMapB,order) <- newBhvTpl M.empty
                      -- listening the requests stream 
                      listenTrans (filterJust $ snapshot onNewPipe npE pMapB) order

                      -- listening the deconnections stream
                      listenTrans (M.delete <$> decoE pMapB) order
                      pure (pMapB,order)
    where onNewPipe :: (Request, Event PipeMessage) -> PipeMap -> Maybe (PipeMap -> PipeMap)
          onNewPipe (req,pE) pMap = case reqPipeID req `M.lookup` pMap of
                                            Just _ -> Nothing
                                            Nothing -> Just $ M.insert (reqPipeID req) pE
          decoE :: Behaviour PipeMap -> Event PipeID
          decoE pMapB = fst . fromLeft <$> (filterE isLeft $ allEvents pMapB)

type PipeManager = M.Map SourceID (BhvTpl PipeMap)
buildPipeManager :: Behaviour RecipientMap -> Reactive (Behaviour PipeManager)
buildPipeManager rMapB = swapBRM $ fmap (buildPipeMap . eEvent) <$> rMapB

type DataManager = EventMap SourceID RawData
buildDataManager :: Behaviour PipeManager -> Behaviour DataManager
buildDataManager pManaB = fmap (fmap (snd . fromRight) . filterE isRight . allEvents) <$> pManaB
