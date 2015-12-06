module Client where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Monad
import Data.Binary
import qualified Data.Map as M

import Ressource
import Neighbors
import Routing
import PipePackets
import Pipes
import Ed25519
import Crypto
import Class
import Timer


type Packet = Either NeighPacket PipePacket 


data Client = Client {clNeighbors :: Neighborhood,
                      clRouting :: Routing,
                      clRessources :: Ressources,
                      clDHKeys :: DHKeyPair,
                      clKeys :: KeyPair,
                      clUserID :: UserID,
                    
                      clReceived :: Event Packet,
                      clToSend :: Event Packet,
                      clSendH :: Handler Packet,
                      clLogs :: Event String,
  
                      clNewRoadH :: Handler NewRoad,
                      clSendToPeer :: Handler (SourceID, RawData)
                      }

buildClient :: Event Packet -> DHKeyPair -> KeyPair -> UserID -> MomentIO Client
buildClient packetsE (dhPK,dhSK) (pK,sK) uID = do 
                          let (neighPE, pipesPE) = split packetsE
                          (newRoadE, newRoadH) <- newEvent
                          (locMsgE, locMsgH) <- newEvent
                          (sendE,sendH) <- newEvent

                        
                          liftIO . print $ "building Modules"

                          neighs <- buildNeighborhood neighPE
                          res <- buildRessources dhPK uID (pK, sK) (nbhRessources neighs) 
                          rout <- buildRouting uID dhSK newRoadE (nbhRequests neighs) pipesPE (nbhNeighBreak neighs) 

                          liftIO . print $ "connecting handlers"
                          ansE <- mergeEvents . bcChanges $ resListenMap res
                          reactimate $ newRoadH . answerToNewRoad uID <$> ansE

                          liftIO . print $ "starting neighIntro repeater"
                          (stopRepeatIntro, introE) <- repeatNeighIntro neighRepeatTime uID (pK,sK) emptyPayload
                          
                          neighDataE <- unionM $ (sendNeighData uID (pK,sK) <$>) <$>  [routingOutgoingNeighPacket rout,
                                                                                      NeighRes <$> resRelPackets res] 
                          neighPacketE <- unionM [neighDataE, introE]
                          toSend <- unionM [sendE, Left <$> neighPacketE , Right <$> routingOutgoingPackets rout]
                          sendLocalMessages rout locMsgE
                          logsE <- unionM [routingLogs rout, resLogs res]
                          pure $ Client neighs rout res (dhPK, dhSK) (pK,sK) uID packetsE toSend sendH logsE newRoadH locMsgH 
--                          pure $ Client neighs (dhPK, dhSK) (pK,sK) uID packetsE toSend 


clSendNeighData :: Client -> Handler NeighDataContent
clSendNeighData c d = clSendH c . Left $ ((sendNeighData <$> clUserID <*> clKeys <*> pure d) $ c)

clSendResearch :: Client -> Handler RessourceID 
clSendResearch c = clSendNeighData c . NeighRes . genResearch
clResearch :: Client -> RessourceID -> IO ()
clResearch c rID = do putStrLn $ "beginning research for : " ++ show rID
                      void $ newRepeater Nothing 5 $ clSendResearch c rID
                      putStrLn $ "calling ressource handler"
                      resListenHandler (clRessources c) (rID, True)
                      putStrLn $ "done"


clSendAnswer :: Client -> Handler (Time, RawData, RessourceID)
clSendAnswer c (t,d,rid) = sendAnswer (fst $ clDHKeys c) (clKeys c) (clUserID c) (clSendNeighData c . NeighRes) (t,d) rid


answerToNewRoad :: UserID -> Answer -> NewRoad
answerToNewRoad uID = NewRoad <$> (uID :) . ansRoad <*> cResSourceDHKey . ansCert <*> ansSourceID <*> pure (encode "wooobdidoo")

