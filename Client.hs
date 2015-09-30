module Client where

import FRP.Sodium
import FRP.Sodium.Internal hiding (Event)
import qualified Data.Map as M

import Ressource
import Neighbors
import Routing
import Pipes
import Ed25519
import Class
import Crypto

--type Packet = PacketNeighI NeighIntro | PacketNeighD NeighData | PacketPipe PipePacket

data PacketEvents = PacketEvents {neighIntroE :: Event NeighIntro,
                                  neighDataE :: Event NeighData,
                                  pipePacketE :: Event PipePacket}

data Client = Client {clNeighbors :: Neighborhood,
                      clRouting :: Routing,
                      clRessources :: Ressources,
                      clPipes :: Pipes,
                      clDHKeys :: (DHPubKey, DHPrivKey),
                      clKeys :: (PubKey, PrivKey),
                      clUserID :: UserID,
                      
                      clToSend :: PacketEvents,

                      clLocResH :: Handler RessourceID}

buildClient :: PacketEvents -> Reactive Client
buildClient packetsE = do (pK, sK) <- ioReactive generateKeyPair
                          (dhPK, dhSK) <- ioReactive generateDHKeyPair
                          let uID = computeHashFromKey pK
                        
                          (locResE, locResH) <- newEvent'

                          neighs <- buildNeighborhood (neighIntroE packetsE) (neighDataE packetsE)
                          res <- buildRessources dhPK (pK, sK) M.empty locResE (nbhResearchs neighs) (nbhAnswers neighs)
                          rout <- buildRouting (pK,sK) (nbhRequests neighs) (pipePacketE packetsE)
                          pipes <- buildPipes 
                          listenTrans (routingNewPipes rout) $ fire $ pipeNewPipe pipes
                        
                          let toSend = PacketEvents never never $ merge (pipesMessagesOut pipes) (routingRelayedPackets rout)  

                          pure $ Client neighs rout res pipes (dhPK, dhSK) (pK,sK) uID toSend locResH





