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
  
                        clSendToPeer :: Handler (SourceID, RawData),
                        clLocResH :: Handler RessourceID}

buildClient :: Frameworks t => Event t Packet -> Moment t (Client t)
buildClient packetsE = do (pK, sK) <- liftIO generateKeyPair
                          (dhPK, dhSK) <- liftIO generateDHKeyPair
                          let uID = computeHashFromKey pK

                          (neighPE, pipesPE) <- splitEither packetsE
                          (locResE, locResH) <- newEvent
                          (locMsgE, locMsgH) <- newEvent

                          neighs <- buildNeighborhood neighPE
                          res <- buildRessources dhPK (pK, sK) M.empty (nbhResearchs neighs) (nbhAnswers neighs)
                          rout <- buildRouting uID (pK,sK) (nbhRequests neighs) pipesPE
                          pipes <- buildPipes 
                          reactimate $ pipeNewPipe pipes <$> routingNewPipes rout
                          sendToSource pipes locMsgE
                          reactimate $ resResearchHandle res <$> locResE
                        
                          let toSend = Right <$> union (pipesMessagesOut pipes) (routingRelayedPackets rout)  

                          pure $ Client neighs rout res pipes (dhPK, dhSK) (pK,sK) uID packetsE toSend locMsgH locResH




