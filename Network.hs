{-# LANGUAGE ImpredicativeTypes, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
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
import PipePackets
import Pipes

type ClientShow = [(String, [AddHandler String])]
type Logs = String

data ClientInterface = ClientInterface {ciInput :: Handler Packet,
                                        ciOutput :: AddHandler Packet,
                                        ciIDs :: (UserID, KeyPair, DHKeyPair),
                                        ciResearch :: Handler RessourceID,
                                        ciOfferRessource :: Handler (Time, Payload, RessourceID),
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
                                  cleResAnswerMap :: AddHandler AnswerMap,
                                  cleResLocalMap :: AddHandler LocalAnswerMap,
                                  cleResListenMap :: AddHandler (EventMap RessourceID Answer),
                                  cleRoutingLocalMap :: AddHandler (EventMap UserID PipePacket),
                                  cleRoutingRelayedMap :: AddHandler (EventMap UserID PipePacket),
                                  cleReceivedMessage :: AddHandler Packet,
                                  cleRoutingLogs :: AddHandler Logs,
                                  clePipeManager :: AddHandler (M.Map SourceID PipesMap),
                                  clePipeLogs :: AddHandler Logs}

compileClient :: IO ClientInterface
compileClient = do 
                               (inE,inH) <- nah
                               (keys,dhKeys) <- (,) <$> generateKeyPair <*> generateDHKeyPair
                               let uID = computeHashFromKey $ fst keys
                                   ids = (uID, keys, dhKeys)
                                                               
                               (ci,act) <- runWriterT $ ClientInterface inH <$> eah clToSend
                                                                            <*> pure ids
                                                                            <*> eh clResearch
                                                                            <*> eh (offerRessource . clRessources)
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
                                                                            <*> (ClientEvents   <$> ec (nbhNeighMap . clNeighbors)
                                                                                                <*> ( (,) <$> eah (nbhRequests . clNeighbors) 
                                                                                                          <*> eah (nbhRessources . clNeighbors) )
                                                                                                <*> eah (bmChanges . resAnswerMap . clRessources)
                                                                                                <*> eah (bmChanges . resLocalAnswerMap . clRessources)
                                                                                                <*> eah (resListenMap . clRessources)
                                                                                                <*> ec (routingLocMap . clRouting)
                                                                                                <*> ec (routingRelMap . clRouting)
                                                                                                <*> eah clReceived 
                                                                                                <*> eah (routingLogs . clRouting)
                                                                                                <*> eah exctractPipeManager
                                                                                                <*> eah (pipesLogs . clPipes) )



                               let mkClient :: Frameworks t => Moment t ()
                                   mkClient = do inEv <- fromAddHandler inE 
                                                 c <- buildClient inEv dhKeys keys uID
                                                 forM_ act $ \a -> getBA a $ c
                                                 pure ()
                               actuate =<< compile mkClient 
                               
                               pure ci

    where 
          nah = newAddHandler
          eah ::  BananEvent a -> WBanan (AddHandler a)
          eah = extractEvent
          exctractPipeManager :: Client t -> Event t (M.Map SourceID PipesMap)
          exctractPipeManager c = (pmePipeMap <$>) <$> (bmChanges . pipesManager $ clPipes c)

          em :: (forall t. Frameworks t => Client t -> BehaviorMod t (EventEntryMap k a)) -> WBanan (AddHandler (EventMap k a))
          em f = eah (((eAddHandler <$>) <$>) .  bmChanges . f)
          ec :: (forall t. Frameworks t => Client t -> BehaviorC t (EventCMap k a)) -> WBanan (AddHandler (EventMap k a))
          ec f = eah (((ceAddHandler <$>) <$>) .  bcChanges . f)
          eh :: BananHandler a -> WBanan (Handler a)
          eh = extractHandler


newtype BananAction = BananAction {getBA :: forall t. Frameworks t => Client t -> Moment t ()}
type BananEvent a = forall t. Frameworks t => Client t -> Event t a
type BananHandler a = forall t. Frameworks t => Client t -> Handler a
type WBanan = WriterT [BananAction] IO

extractHandler :: BananHandler a -> WBanan (Handler a)
extractHandler f = do (ah,h) <- liftIO newAddHandler
                      let b :: forall t. Frameworks t => Client t -> Moment t ()
                          b c = do liftIO . void $ register ah (f c)
                      tell [BananAction b]
                      pure h

extractEvent ::  BananEvent a -> WBanan (AddHandler a)
extractEvent f = do (ah,h) <- liftIO newAddHandler
                    let b :: forall t. Frameworks t => Client t -> Moment t ()
                        b c = reactimate $ h <$> f c
                    tell [BananAction b]
                    pure ah


{-
test :: AddHandler a -> (forall t. Frameworks t => Client t -> Handler a) -> BananAction
test ah bh = x
    where x :: forall t. Frameworks t => Client t -> Moment t ()
          x c = do liftIO $ register ah (bh c)
                   pure ()

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
