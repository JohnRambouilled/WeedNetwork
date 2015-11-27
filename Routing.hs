{-# LANGUAGE DeriveGeneric #-}
module Routing where

import Crypto 
import Class
import Timer
import PipePackets

import Reactive.Banana
import Reactive.Banana.Frameworks
import GHC.Generics
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.Clock.POSIX



type RoutingMap = EventCMap UserID PipePacket
type RoutingMapBhv t = BehaviorC t RoutingMap

data NewPipe = NewPipe {npRoad :: Road,
                        npSource :: SourceID,
                        npPubKey :: PubKey,
                        npSender :: PipeMessage -> PipePacket,
                        npPipeID :: PipeID,
                        npTime :: Time,
                        npContent :: RawData,
                        npMessageEvent :: EventC PipeMessage}

data NewRoad = NewRoad {nrRoad :: Road,
                        nrDHPubKey :: DHPubKey,
                        nrSourceID :: SourceID,
                        nrContent :: RawData}

newRequestToNewPipe :: DHPrivKey -> NewRequest -> EventC PipeMessage -> Maybe NewPipe
newRequestToNewPipe uk (IncomingRequest (Request n _ r epk t pK pID _ cnt)) e = (\s -> NewPipe r (head r) pK s pID t cnt e) <$> sender
            where sender = pipeMessageToPipePacket n False <$> decryptKeyPair epk uk
newRequestToNewPipe _ (OutgoingRequest (Request _ _ r _ t pK pID _ cnt) sender) e = Just $ NewPipe r (head r) pK sender pID t cnt e



data Routing t = Routing {routingLocMap :: RoutingMapBhv t,
                          routingRelMap :: RoutingMapBhv t,

                          routingNewPipes :: Event t NewPipe,
                          routingRelayedPackets :: Event t PipePacket,
                          routingOutgoingRequest :: Event t Request,

                          routingLocClose :: Handler PipeID,
                          routingRelClose :: Handler PipeID,

                          routingLogs :: Event t String}

pipeTimeOut = 10 :: Time


buildRouting :: Frameworks t => UserID -> DHPrivKey -> Event t NewRoad -> Event t Request -> Event t PipePacket -> Moment t (Routing t)
buildRouting uID dhSK newRoadE reqEuc packetE = do
                     (reqLogs, reqE) <- splitEither =<< liftIOEvent (checkRequest uID <$> reqEuc) --Checking request validity
                     (reqRelE, reqLocE) <- splitEvent isLocalRequest reqE                         --Splitting local from relayed requests
                     reqOutE <- routOpenPipe newRoadE                                             --Building Request from NewRoad 
                     (relayMap, cryptoRelE) <- buildCryptoMap reqRelE packetE                     --Building Relayed RoutingMap
                     (relRefreshE, relNewE) <- splitEither cryptoRelE
                     buildTimeOut pipeTimeOut relNewE (never :: Event t KeyHash) --[TODO] Refresh
                     let reqRelOE = relayRequest <$> (union relRefreshE $ fst <$> relNewE)       --Request relayed to transmit
                         newReqE = union (IncomingRequest <$> reqLocE) reqOutE                   --Local Request (incoming and outgoing)
                         requestOut = union reqRelOE $ nrReq <$> reqOutE                         --Request output 
                     relayPacketE <- (relayPackets <$>) <$> mergeEvents (bcChanges relayMap)      --Relayed packets output 
                     closePipes relNewE                                                       --Closing relayed pipe on PipeClose packets
                     (localMap, cryptoLocE) <- buildCryptoMap newReqE packetE                     --Building Local RoutingMap 
                     (locRefreshE, locNewE) <- splitEither cryptoLocE
                     let logs = unions [("AcceptedRequest : " ++) . show <$> reqE,
                                        ("Rejected request : " ++) <$> reqLogs]
                         newPipeE = filterJust $ makeNewPipe <$> locNewE                         --Event of NewPipes
                     [relClose, locClose] <- forM [relayMap, localMap] $ buildCloseHandle . bcLastValue     --Generating close handles 
                     pure $ Routing localMap relayMap newPipeE relayPacketE requestOut locClose relClose logs      --Producing output
    where relayPackets :: PipePacket -> PipePacket
          relayPackets p = p{pipePosition = if pipeDirection p then pipePosition p + 1 else pipePosition p - 1} 
          relayRequest :: Request -> Request
          relayRequest r = r{reqPosition = reqPosition r + 1}
          isLocalRequest req = reqPosition req == reqLength req - 1
          closePipes :: Frameworks t => Event t (Request, EventC PipePacket) -> Moment t () 
          closePipes cE = reactimate $ closeP . snd <$> cE
            where closeP eC = void $ ceAddHandler eC `register` filterPipeClose (ceClose eC)
                  filterPipeClose h (PipeClose _ _ _ _ _) = h ()
                  filterPipeClose _ _ = pure ()
          makeNewPipe (req, e ) = newRequestToNewPipe dhSK req $ makePipeMessage <$> e
          
          

routOpenPipe :: Frameworks t => Event t NewRoad -> Moment t (Event t NewRequest)
routOpenPipe newRE = do 
                        (reqE, reqH) <- newEvent
                        reactimate $ onNewRoad reqH <$> newRE
                        pure $ reqE
    where onNewRoad :: Handler NewRequest -> NewRoad -> IO ()
          onNewRoad send nr = do (dhPK, dhSK) <- generateDHKeyPair
                                 case decryptKeyPair (nrDHPubKey nr) dhSK of
                                        Nothing -> pure ()
                                        Just (pK,sK) -> do t <- getPOSIXTime
                                                           let (r,cnt) = ( (,) <$> nrRoad <*> nrContent ) $ nr
                                                               pID = computeHashFromKey pK
                                                               req = sign (pK,sK) $ Request 1 (length r) r dhPK t pK pID emptySignature cnt
                                                               sender = pipeMessageToPipePacket 1 True (pK,sK)
                                                           send $ OutgoingRequest req sender



