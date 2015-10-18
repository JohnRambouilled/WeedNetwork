{-# LANGUAGE DeriveGeneric #-}
module Pipes where

import Crypto
import Routing 
import Class


import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Data.Binary
import Data.Maybe (isJust, fromJust)
import Control.Monad
import qualified Data.Map as M

type UserID = KeyHash
type PipesSender = Handler PipeMessage
type PipeCloser = Handler (SourceID, PipeID)
type NewSourceEvent t = Event t (SourceID, AddHandler NewPipe)


type PipesMap = M.Map PipeID (AddHandler PipeMessage, PipesSender)

data PipeManagerEntry =  PipeManagerEntry { pmeFireNP :: Handler NewPipe,
                                            pmeUnregister :: IO (),
                                            pmePipeMap :: PipesMap }

type PipesManager = M.Map SourceID PipeManagerEntry
type PipesManagerBhv t = ModEvent t PipesManager

data Pipes t = Pipes {pipesManager :: PipesManagerBhv t,
                      pipeNewSourceEvent :: NewSourceEvent t,
                      pipesMessagesOut :: Event t PipePacket,
                      pipeNewPipe :: Handler NewPipe,
                      pipesClosePipe :: PipeCloser,
                      pipesRemoveSource :: Handler SourceID} 

buildPipes :: Frameworks t => Moment t (Pipes t)
buildPipes = do (closeE, closeH) <- newEvent
                (npE, npH) <- newEvent
                (remSE, remSH) <- newEvent
                (pipeManB, sendE, newSourceE) <- buildPipeManager closeH npE
                reactimate $ closePipe (meModifier pipeManB) <$> closeE
                reactimate $ applyMod (removeSource closeH) pipeManB remSE
                pure $ Pipes pipeManB newSourceE sendE npH closeH remSH
    where closePipe mod (sID, pID) = mod $ M.adjust (deletePipe pID) sID
          removeSource :: PipeCloser -> Modifier PipesManager -> PipesManager -> SourceID -> IO ()
          removeSource closeH manM man sID = case M.lookup sID man of
                                                  Nothing -> pure ()
                                                  Just pme -> do pmeUnregister pme
                                                                 manM $ M.delete sID


type DataManager = EventMap SourceID RawData
--buildDataManager :: Behavior PipesManager -> Behavior DataManager
--buildDataManager pManaB = fmap (fmap (snd . fromRight) . filterE isRight . fst . M.elems . pmePipeMap) <$> pManaB


{-| Converts the map of physical neighbors into a map of recipients |-}
buildPipeManager :: Frameworks t => PipeCloser -> Event t NewPipe -> Moment t (PipesManagerBhv t, Event t PipePacket, NewSourceEvent t)
buildPipeManager closeH npE = do manM <- newModEvent M.empty
                                 (nsE, nsH) <- newEvent
                                 (sendE, sendH) <- newEvent
                                 reactimate $ apply (f nsH sendH (meModifier manM) <$> meLastValue manM) npE
                                 pure (manM, sendE, nsE)
  where f :: Handler (SourceID, AddHandler NewPipe) -> Handler PipePacket -> Modifier PipesManager -> PipesManager -> NewPipe -> IO ()
        f nsH sendH mod rM np = case sID `M.lookup` rM of
                                     Just pme -> pmeFireNP pme $ np
                                     Nothing -> do (e, h) <- newAddHandler
                                                   unreg <- buildPipeMap sendH e pipeMapMod
                                                   mod $ M.insert sID $ PipeManagerEntry h unreg M.empty
                                                   nsH (sID, e)
                                                   h np
                where sID = npSource np
                      pipeMapMod :: (PipesMap -> PipesMap) -> IO ()
                      pipeMapMod f = mod $ M.adjust (\pme -> pme{pmePipeMap = f $ pmePipeMap pme}) sID


buildPipeMap :: Handler PipePacket -> AddHandler NewPipe -> Modifier PipesMap -> IO (IO ())
buildPipeMap sendH npE = register $  onNewPipe <$> npE
    where onNewPipe :: NewPipe -> PipesMap -> PipesMap
          onNewPipe np pM = case pID `M.lookup` pM of
                                Nothing -> M.insert pID (npMessageEvent np, sendH . npSender np) pM
                                Just _ -> pM
            where pID = npPipeID np

deletePipe :: PipeID -> PipeManagerEntry -> PipeManagerEntry
deletePipe pID pme = pme{pmePipeMap = M.delete pID $ pmePipeMap pme}
