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


type ClientEvent a = Client -> Handler a -> MomentIO ()
type ClientHandler a = Client -> Event a -> MomentIO ()
type BananAction = Client -> MomentIO ()
type BananWriter = WriterT [BananAction] IO
type ShowHandle = AddHandler String
type Logs = String

data ClientInterface = ClientInterface{ ciInput :: Handler Packet,
                                        ciOutput :: AddHandler Packet,
                                        ciIDs :: (UserID, KeyPair, DHKeyPair)}

compileClient :: BananWriter a -> IO (ClientInterface, a)
compileClient bwA = do (inE,inH) <- newAddHandler
                       (outE, outH) <- newAddHandler
                       (keys,dhKeys) <- (,) <$> generateKeyPair <*> generateDHKeyPair

                       (a, act) <- runWriterT bwA

                       let uID = computeHashFromKey $ fst keys
                           mkClient :: MomentIO ()
                           mkClient = do inEv <- fromAddHandler inE 
                                         c <- buildClient inEv dhKeys keys uID
                                         reactimate $ outH <$> clToSend c
                                         forM_ act ($c)
                                         pure ()
                           ci = ClientInterface inH outE (uID, keys, dhKeys)
                       actuate =<< compile mkClient 
                       pure (ci,a)


getClientEvent :: ClientEvent a -> BananWriter (AddHandler a)
getClientEvent ce = do (e,h) <- liftIO $ newAddHandler
                       tell [flip ce $ h]
                       pure e
getClientHandler :: ClientHandler a -> BananWriter (Handler a)
getClientHandler ch = do (e,h) <- liftIO $ newAddHandler
                         tell [\c -> ch c =<< fromAddHandler e]
                         pure h


extractAddHandler :: (Client -> Event a) -> BananWriter (AddHandler a)
extractAddHandler f = getClientEvent $ \c h -> reactimate (h <$> f c)

extractHandler :: (Client -> Handler a) -> BananWriter (Handler a)
extractHandler f = getClientHandler ch
    where ch c e = reactimate $ f c <$> e

extractKeyList :: (Client -> BehaviorC (M.Map k a)) -> BananWriter (AddHandler [k])
extractKeyList f = extractAddHandler $ fmap M.keys . bcChanges . f

{-
data ClientInterface = ClientInterface {ciInput :: Handler Packet,
                                        ciOutput :: AddHandler Packet,
                                        ciIDs :: (UserID, KeyPair, DHKeyPair),
                                        ciResearch :: Handler RessourceID,
                                        ciOfferRessource :: Handler (Time, Payload, RessourceID),
                                        ciSend :: Handler (SourceID, RawData),
                                        ciHandlers :: ClientHandlers,
                                        ciEvents :: ClientEvents}

data ClientHandlers = ClientHandlers {clhSendResearch :: Handler RessourceID,
                                      clhSendAnswer :: Handler (Time, RawData, RessourceID),
                                      clhOpenPipe :: Handler NewRoad,
                                      ciSendOnPipe :: Handler (SourceID, PipeID, RawData),
                                      clhSendNeighIntro :: Handler RawData,
                                      clhBanNeighbor :: Handler UserID,
                                      clhCloseLocPipe :: Handler PipeID,
                                      clhCloseRelPipe :: Handler PipeID,
                                      clhCloseSource :: Handler SourceID}

data ClientEvents = ClientEvents {cleNeighborsList :: AddHandler [UserID],
                                  cleNeighborsData :: (AddHandler Request, AddHandler RessourcePacket),
                                  cleResAnswerMap :: AddHandler AnswerMap,
                                  cleResLocalMap :: AddHandler LocalAnswerMap,
                                  cleResListenList :: AddHandler [RessourceID],
                                  cleRoutingLocalList :: AddHandler [PipeID],
                                  cleRoutingRelayedList :: AddHandler [PipeID],
                                  cleReceivedMessage :: AddHandler Packet,
                                  cleRoutingLogs :: AddHandler Logs}

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
                                                                            <*> (ClientHandlers <$> eh clSendResearch
                                                                                                <*> eh clSendAnswer
                                                                                                <*> eh clNewRoadH
                                                                                                <*> ech (sendOnPipe . clRouting)
                                                                                                <*> eh (\c -> clSendH c . Left . sendNeighIntro uID keys)
                                                                                                <*> eh (nbhForceDeco . clNeighbors)
                                                                                                <*> eh (routingLocClose . clRouting)
                                                                                                <*> eh (routingRelClose . clRouting)
                                                                                                <*> ech (closeSources . clRouting) )
                                                                            <*> (ClientEvents   <$> ekl (nbhNeighMap . clNeighbors)
                                                                                                <*> ( (,) <$> eah (nbhRequests . clNeighbors) 
                                                                                                          <*> eah (nbhRessources . clNeighbors) )
                                                                                                <*> eah (bmChanges . resAnswerMap . clRessources)
                                                                                                <*> eah (bmChanges . resLocalAnswerMap . clRessources)
                                                                                                <*> ekl (resListenMap . clRessources)
                                                                                                <*> ekl (routingLocMap . clRouting)
                                                                                                <*> ekl (routingRelMap . clRouting)
                                                                                                <*> eah clReceived 
                                                                                                <*> eah (routingLogs . clRouting))



                               let mkClient :: MomentIO ()
                                   mkClient = do inEv <- fromAddHandler inE 
                                                 c <- buildClient inEv dhKeys keys uID
                                                 forM_ act $ \a -> a $ c
                                                 pure ()
                               actuate =<< compile mkClient 
                               
                               pure ci

    where 
          nah = newAddHandler
          eah = extractAddHandler
          eh = extractHandler
          ece = getClientEvent
          ech = getClientHandler
          ekl = extractKeyList
-}

