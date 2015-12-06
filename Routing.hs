{-# LANGUAGE DeriveGeneric #-}
module Routing where

import Crypto 
import Class
import Timer
import Neighbors
import PipePackets
import Pipes
import Routes.Core

import Control.Lens
import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Monad
import Data.Time.Clock.POSIX
import qualified Data.Map as M



type RoutingMap = EventCMap PipeID PipePacket
type RoutingMapBhv = BehaviorC RoutingMap

data NewRoad = NewRoad {nrRoad :: Road,
                        nrDHPubKey :: DHPubKey,
                        nrSourceID :: SourceID,
                        nrContent :: RawData}

newRequestToNewPipe :: Handler PipePacket -> DHPrivKey -> NewRequest -> EventC PipeMessage -> Maybe NewPipe
newRequestToNewPipe sendH uk (IncomingRequest (Request n _ r epk t pK pID _ cnt)) e = (\s -> NewPipe r (head r) pK s pID t cnt e) <$> sender
            where sender = (sendH <$>) . pipeMessageToPipePacket n False <$> decryptKeyPair epk uk 
newRequestToNewPipe sendH _ (OutgoingRequest (Request _ _ r _ t pK pID _ cnt) sender) e = Just $ NewPipe r (head r) pK (sendH . sender) pID t cnt e



data Routing = Routing {routingLocMap :: RoutingMapBhv,
                        routingRelMap :: RoutingMapBhv, 
                        routingSourceMap :: BehaviorC SourceMap,
                        routingTree :: BehaviorC RoutingTree,

                        routingOutgoingPackets :: Event PipePacket,
                        routingOutgoingNeighPacket :: Event NeighDataContent,

                        routingLocClose :: Handler PipeID,
                        routingRelClose :: Handler PipeID,

                        routingLogs :: Event String}



buildRouting :: UserID -> DHPrivKey -> Event NewRoad -> Event Request -> Event PipePacket -> Event NeighBreak -> MomentIO Routing
buildRouting uID dhSK newRoadE reqEuc packetE neighBE = do
                     (packetOutE, packetOutH) <- newEvent
                     (reqLogs, reqE) <- split <$> liftIOEvent (checkRequest uID <$> reqEuc)       --Checking request validity
                     let (reqRelE, reqLocE) = splitEvent isLocalRequest reqE                     --Splitting local from relayed requests
                     reqOutE <- routOpenPipe newRoadE                                             --Building Request from NewRoad 
                     (relayMap, cryptoRelE) <- buildCryptoMap reqRelE packetE                     --Building Relayed RoutingMap
                     let (relRefreshE, relNewE) = split cryptoRelE
                     buildTimeOutIDable pipeTimeOut relNewE (never :: Event KeyHash)            --[TODO] Refresh
                     newReqE <- unionM [IncomingRequest <$> reqLocE, reqOutE]                     --Local Request (incoming and outgoing)
                     closePipes relNewE                                                          --Closing relayed pipe on PipeClose packets
                     (localMap, cryptoLocE) <- buildCryptoMap newReqE packetE                     --Building Local RoutingMap 
                     let (locRefreshE, locNewE) = split cryptoLocE
                         newPipeE = filterJust $ makeNewPipe packetOutH <$> locNewE              --Event of NewPipes
                     buildTimeOutIDable pipeTimeOut locNewE (never :: Event KeyHash)
                     newReqE <- unionM [relNewE, over _1 nrReq <$> locNewE ]
                     (routTree, nbOutE) <- buildRoutingTable uID newReqE neighBE
                     requestOut <- unionM [NeighReq . relayRequest <$> relRefreshE,
                                          NeighReq . relayRequest . fst <$> relNewE,
                                          NeighReq . nrReq <$> reqOutE,
                                          NeighBrk <$> nbOutE ]                                     --Request output 
                     relayPacketE <- fmap relayPackets <$> mergeEvents (bcChanges relayMap)       --Relayed packets output 
                     packetsOut <- unionM [packetOutE, relayPacketE]                       
                     logs <- unionM [("AcceptedRequest : " ++) . show <$> reqE,
                                    ("Rejected request : " ++) <$> reqLogs]
                     [relClose, locClose] <- forM [relayMap, localMap] $ buildCloseHandle . bcLastValue     --Generating close handles 
                     sourceMap <- buildSourceMap newPipeE
                     pure $ Routing localMap relayMap sourceMap routTree packetsOut requestOut locClose relClose logs      --Producing output
    where relayPackets :: PipePacket -> PipePacket
          relayPackets p = p{pipePosition = if pipeDirection p then pipePosition p + 1 else pipePosition p - 1} 
          relayRequest :: Request -> Request
          relayRequest r = r{reqPosition = reqPosition r + 1}
          isLocalRequest req = reqPosition req == reqLength req - 1
          closePipes :: Event (Request, EventC PipePacket) -> MomentIO () 
          closePipes cE = void . execute $ closeP . snd <$> cE
            where closeP eC = reactimate $ filterPipeClose (ceClose eC) <$> ceEvent eC
                  filterPipeClose h (PipeClose _ _ _ _ _) = h ()
                  filterPipeClose _ _ = pure ()
          makeNewPipe :: Handler PipePacket -> (NewRequest, EventC PipePacket) -> Maybe NewPipe
          makeNewPipe sendH (req, e ) = newRequestToNewPipe sendH dhSK req $ makePipeMessage <$> e
          
          

routOpenPipe :: Event NewRoad -> MomentIO (Event NewRequest)
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


sendLocalMessages :: Routing -> Event (SourceID, Payload) -> MomentIO ()
sendLocalMessages rout = reactimate . apply sendB 
    where sendB = sendToSource <$> bcLastValue (routingSourceMap rout)


sendOnPipe :: Routing -> Event (SourceID, PipeID, Payload) -> MomentIO ()
sendOnPipe rout e = do (b,_) <- newBehavior M.empty
                       reactimate $ apply (send <$> pipeMapB b) e
    where pipeMapB :: Behavior PipeMap -> Behavior PipeMap
          pipeMapB b = switchB b . filterJust $ apply (getPM <$> bcLastValue (routingSourceMap rout)) e
          getPM sm (sID, _,_) = bcLastValue . sePipeMap <$> sID `M.lookup` sm
          send pm (_, pID, d) = case pID `M.lookup` pm of
                                    Nothing -> pure ()
                                    Just pe -> peSender pe $ Right (pID,d)


closeSources :: Routing -> Event SourceID -> MomentIO ()
closeSources rout = reactimate . apply (close <$> bcLastValue (routingSourceMap rout))
    where close m sID = maybe (pure ()) (($ ()) . ceClose . seReceiver)   $ sID `M.lookup` m

