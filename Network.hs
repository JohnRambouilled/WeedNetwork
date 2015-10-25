module Network where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M
import Control.Monad
import Control.Concurrent
import Data.Binary

--import UI.ShowClient
import UI.App
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
                                        ciIDs :: (UserID, KeyPair),
                                        ciResearch :: Handler RessourceID,
                                        ciSend :: Handler (SourceID, RawData),
                                        ciSendOnPipe :: Handler (PipeID, RawData),
                                        ciReceive :: AddHandler DataManager,
                                        ciHandlers :: ClientHandlers,
                                        ciEvents :: ClientEvents}

data ClientHandlers = ClientHandlers {clhSendResearch :: Handler RessourceID,
                                      clhSendAnswer :: Handler RessourceID,
                                      clhOpenPipe :: Handler NewRoad,
                                      clhSendNeighIntro :: Handler RawData,
                                      clhBanNeighbor :: Handler UserID,
                                      clhClosePipe :: Handler PipeID,
                                      clhCloseSource :: Handler SourceID}

data ClientEvents = ClientEvents {cleNeighborsMap :: AddHandler (EventMap KeyHash NeighData),
                                  cleNeighborsData :: (AddHandler Request, AddHandler RessourcePacket),
                                  cleRoutingLocalMap :: AddHandler (EventMap UserID PipePacket),
                                  cleRoutingRelayedMap :: AddHandler (EventMap UserID PipePacket),
                                  cleRoutingLogs :: AddHandler Logs,
                                  clePipeManager :: AddHandler (M.Map SourceID PipesMap),
                                  clePipeLogs :: AddHandler Logs}




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
