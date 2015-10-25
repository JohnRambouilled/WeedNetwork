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


type Packet = Either NeighPacket PipePacket 


data Client t = Client {clNeighbors :: Neighborhood t,
                        clRouting :: Routing t,
                        clRessources :: Ressources t,
                        clPipes :: Pipes t,
                        clDHKeys :: (DHPubKey, DHPrivKey),
                        clKeys :: (PubKey, PrivKey),
                        clUserID :: UserID,
                      
                        clReceived :: Event t Packet,
                        clToSend :: Event t Packet,
  
                        clSendToPeer :: Handler (SourceID, RawData)}

buildClient :: Frameworks t => Event t Packet -> Event t RessourceID -> [RessourceID] -> Moment t (Client t)
buildClient packetsE locResE rIDL = do 
                          liftIO . print $ "Key generation"
                          (pK, sK) <- liftIO generateKeyPair
                          (dhPK, dhSK) <- liftIO generateDHKeyPair
                          let uID = computeHashFromKey pK

                          (neighPE, pipesPE) <- splitEither packetsE
                          (newRoadE, newRoadH) <- newEvent
                          (locMsgE, locMsgH) <- newEvent
                        
                          liftIO . print $ "building Modules"

                          neighs <- buildNeighborhood neighPE
                          res <- buildRessources dhPK uID (pK, sK) (nbhRessources neighs) 
                          rout <- buildRouting uID dhSK newRoadE (nbhRequests neighs) pipesPE
                          pipes <- buildPipes $ routingNewPipes rout

                          liftIO . print $ "connecting handlers"
                          sendToSource pipes locMsgE
                          ansE <- mergeEvents $ resListenMap res
                          reactimate $ newRoadH . answerToNewRoad uID <$> ansE

                          liftIO . print $ "starting neighIntro repeater"
                          (stopRepeatIntro, introE) <- repeatNeighIntro neighRepeatTime uID (pK,sK) emptyPayload
                        
                          let toSend = union (Left <$> union dataE introE) $ Right <$> union (pipesMessagesOut pipes) (routingRelayedPackets rout)  
                              dataE = unions $ (sendNeighData uID (pK,sK) <$>) <$>  [NeighReq <$> routingOutgoingRequest rout,
                                                                                     NeighRes <$> resRelPackets res] 
                                            

                          pure $ Client neighs rout res pipes (dhPK, dhSK) (pK,sK) uID packetsE toSend locMsgH 




