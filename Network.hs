module Network where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M
import Control.Monad
import Control.Concurrent
import Data.Binary
import Control.Monad.Writer

--import UI.ShowClient
--import UI.App
import Class
import Client
import Ed25519
import Crypto
import Neighbors
import Timer
import Ressource
import Routing
import Pipes

type ClientShow = [(String, [AddHandler String])]
type Logs = String

data ClientInterface = ClientInterface {ciInput :: Handler Packet,
                                        ciOutput :: AddHandler Packet,
                                        ciIDs :: (UserID, KeyPair, DHKeyPair),
                                        ciResearch :: Handler RessourceID,
                                        ciSend :: Handler (SourceID, RawData),
                                        ciReceive :: AddHandler DataManager,
                                        ciHandlers :: ClientHandlers,
                                        ciEvents :: ClientEvents}

data ClientHandlers = ClientHandlers {clhSendResearch :: Handler RessourceID,
                                      clhSendAnswer :: Handler (Time, RawData, RessourceID),
                                      clhOpenPipe :: Handler NewRoad,
                                      ciSendOnPipe :: Handler (SourceID, PipeID, RawData),
                                      clhSendNeighIntro :: Handler RawData,
                                      clhBanNeighbor :: Handler UserID,
                                      clhClosePipe :: Handler (SourceID, PipeID),
                                      clhCloseSource :: Handler SourceID}

data ClientEvents = ClientEvents {cleNeighborsMap :: AddHandler (EventMap KeyHash NeighData),
                                  cleNeighborsData :: (AddHandler Request, AddHandler RessourcePacket),
                                  cleRoutingLocalMap :: AddHandler (EventMap UserID PipePacket),
                                  cleRoutingRelayedMap :: AddHandler (EventMap UserID PipePacket),
                                  cleRoutingLogs :: AddHandler Logs,
                                  clePipeManager :: AddHandler (M.Map SourceID PipesMap),
                                  clePipeLogs :: AddHandler Logs}

compileClient :: BananAction t -> IO () --ClientInterface
compileClient bananAction = do (inE,inH) <- nah
                               (keys,dhKeys) <- (,) <$> generateKeyPair <*> generateDHKeyPair
                               let uID = computeHashFromKey $ fst keys
                                   ids = (uID, keys, dhKeys)
                                                               
                               (ci,act) <- runWriterT $ ClientInterface inH <$> eah clToSend
                                                                            <*> pure ids
                                                                            <*> eh clSendResearch
                                                                            <*> eh clSendToPeer 
                                                                            <*> eah (pipesDataManager . clPipes)
                                                                            <*> (ClientHandlers <$> eh clSendResearch
                                                                                                <*> eh clSendAnswer
                                                                                                <*> eh clNewRoadH
                                                                                                <*> eh (pipesSendOnPipe . clPipes)
                                                                                                <*> eh (\c -> clSendH c . Left . sendNeighIntro uID keys)
                                                                                                <*> eh (nbhForceDeco . clNeighbors)
                                                                                                <*> eh (pipesClosePipe . clPipes)
                                                                                                <*> eh (pipesRemoveSource . clPipes) )
                                                                            <*> (ClientEvents   <$> em (nbhNeighMap . clNeighbors)
                                                                                                <*> ( (,) <$> eah (nbhRequests . clNeighbors) 
                                                                                                          <*> eah (nbhRessources . clNeighbors) )
                                                                                                <*> em (routingLocMap . clRouting)
                                                                                                <*> em (routingRelMap . clRouting)
                                                                                                <*> eah (routingLogs . clRouting)
                                                                                                <*> eah exctractPipeManager
                                                                                                <*> eah (pipesLogs . clPipes) )



                               let mkClient :: Frameworks t => Moment t ()
                                   mkClient = do inEv <- fromAddHandler inE 
                                                 c <- buildClient inEv dhKeys keys uID
                                                 pure ()
                               --actuate =<< compile mkClient 
                               
                               pure ()

    where 
          nah = newAddHandler
          eah = extractEvent
          exctractPipeManager :: Client t -> Event t (M.Map SourceID PipesMap)
          exctractPipeManager c = (pmePipeMap <$>) <$> (meChanges . pipesManager $ clPipes c)

          em :: (Client t -> ModEvent t (EventEntryMap k e)) -> WriterT (BananAction t) IO (AddHandler (EventMap k e))
          em f = eah (((eAddHandler <$>) <$>) .  meChanges . f)
          eh = extractHandler

type BananAction t = Client t -> Moment t ()
extractHandler :: (Client t -> Handler a) -> WriterT (BananAction t) IO (Handler a)
extractHandler f = do (ah,h) <- liftIO newAddHandler
                      let b c = liftIO $ ah `register` f c
                      tell b
                      pure h

extractEvent :: Frameworks t => (Client t -> Event t a) -> WriterT (BananAction t) IO (AddHandler a)
extractEvent f = do (ah,h) <- liftIO newAddHandler
                    let b c = reactimate $ h <$> f c
                    tell b
                    pure ah





{-
compileClient ::[WidgetHandler] -> IO (AddHandler Packet, Handler Packet)
compileClient = (f <$>) . compileClientRes []
    where f (a,b,_) = (a,b)

compileClientRes :: [RessourceID] -> [WidgetHandler] -> IO (AddHandler Packet, Handler Packet, Handler RessourceID)
compileClientRes rIDL wL = do (inE,inH) <- newAddHandler
                              (outE,outH) <- newAddHandler
                              (resE,resH) <- newAddHandler
                              print "building Client"
                              actuate =<< compile (cClient resE outH inE)
                              print "done"
                              pure (outE, inH, resH)
                      
    where cClient :: Frameworks t => AddHandler RessourceID -> Handler Packet -> AddHandler Packet -> Moment t ()
          cClient rh s h = do packetE <- fromAddHandler h
                              resE <- fromAddHandler rh
                              c <- buildClient packetE resE rIDL
                              reactimate $ s <$> clToSend c
                              showClient c wL


-}
