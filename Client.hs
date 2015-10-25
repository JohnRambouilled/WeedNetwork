module Client where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M

import Ressource
import Neighbors
import Routing
import Pipes
import Ed25519
import Crypto
import Class
import Timer


type Packet = Either NeighPacket PipePacket 


data Client t = Client {clNeighbors :: Neighborhood t,
                        clRouting :: Routing t,
                        clRessources :: Ressources t,
                        clPipes :: Pipes t,
                        clDHKeys :: DHKeyPair,
                        clKeys :: KeyPair,
                        clUserID :: UserID,
                      
                        clReceived :: Event t Packet,
                        clToSend :: Event t Packet,
                        clSendH :: Handler Packet,
  
                        clNewRoadH :: Handler NewRoad,
                        clSendToPeer :: Handler (SourceID, RawData)}

buildClient :: Frameworks t => Event t Packet -> DHKeyPair -> KeyPair -> UserID -> Moment t (Client t)
buildClient packetsE (dhPK,dhSK) (pK,sK) uID = do 
                          (neighPE, pipesPE) <- splitEither packetsE
                          (newRoadE, newRoadH) <- newEvent
                          (locMsgE, locMsgH) <- newEvent
                          (sendE,sendH) <- newEvent
                        
                          liftIO . print $ "building Modules"

                          neighs <- buildNeighborhood neighPE
                          res <- buildRessources dhPK uID (pK, sK) (nbhRessources neighs) 
                          rout <- buildRouting uID dhSK newRoadE (nbhRequests neighs) pipesPE
                          pipes <- buildPipes $ routingNewPipes rout

                          liftIO . print $ "connecting handlers"
                          ansE <- mergeEvents $ resListenMap res
                          reactimate $ newRoadH . answerToNewRoad uID <$> ansE

                          liftIO . print $ "starting neighIntro repeater"
                          (stopRepeatIntro, introE) <- repeatNeighIntro neighRepeatTime uID (pK,sK) emptyPayload
                        
                          let toSend = unions [sendE, Left <$> union dataE introE, Right <$> union (pipesMessagesOut pipes) (routingRelayedPackets rout)]
                              dataE = unions $ (sendNeighData uID (pK,sK) <$>) <$>  [NeighReq <$> routingOutgoingRequest rout,
                                                                                     NeighRes <$> resRelPackets res] 
                                            

                          pure $ Client neighs rout res pipes (dhPK, dhSK) (pK,sK) uID packetsE toSend sendH newRoadH locMsgH 


clSendNeighData :: Client t -> Handler NeighDataContent
clSendNeighData c d = clSendH c . Left $ ((sendNeighData <$> clUserID <*> clKeys <*> pure d) $ c)

clSendResearch :: Client t -> Handler RessourceID 
clSendResearch c = clSendNeighData c . NeighRes . genResearch


clSendAnswer :: Client t -> Handler (Time, RawData, RessourceID)
clSendAnswer c (t,d,rid) = sendAnswer (fst $ clDHKeys c) (fst $ clKeys c) (clUserID c) (clSendNeighData c . NeighRes) (t,d) rid
