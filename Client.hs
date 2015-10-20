module Client where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M

import Ressource
import Neighbors
import Routing
import Pipes
import Ed25519
import Class
import Crypto

--type Packet = PacketNeighI NeighIntro | PacketNeighD NeighData | PacketPipe PipePacket

data PacketEvents t = PacketEvents {neighIntroE :: Event t NeighIntro,
                                    neighDataE :: Event t NeighData,
                                    pipePacketE :: Event t PipePacket}


data Client t = Client {clNeighbors :: Neighborhood t,
                        clRouting :: Routing t,
                        clRessources :: Ressources t,
                        clPipes :: Pipes t,
                        clDHKeys :: (DHPubKey, DHPrivKey),
                        clKeys :: (PubKey, PrivKey),
                        clUserID :: UserID,
                      
                        clToSend :: PacketEvents t,
  
                        clSendToPeer :: Handler (SourceID, RawData),
                        clLocResH :: Handler RessourceID}

buildClient :: Frameworks t => PacketEvents t -> Moment t (Client t)
buildClient packetsE = do (pK, sK) <- liftIO generateKeyPair
                          (dhPK, dhSK) <- liftIO generateDHKeyPair
                          let uID = computeHashFromKey pK
                        
                          (locResE, locResH) <- newEvent
                          (locMsgE, locMsgH) <- newEvent

                          neighs <- buildNeighborhood (neighIntroE packetsE) (neighDataE packetsE)
                          res <- buildRessources dhPK (pK, sK) M.empty (nbhResearchs neighs) (nbhAnswers neighs)
                          rout <- buildRouting uID (pK,sK) (nbhRequests neighs) (pipePacketE packetsE)
                          pipes <- buildPipes 
                          reactimate $ pipeNewPipe pipes <$> routingNewPipes rout
                          reactimate $ resResearchHandle res <$> locResE
                        
                          let toSend = PacketEvents never never $ union (pipesMessagesOut pipes) (routingRelayedPackets rout)  

                          pure $ Client neighs rout res pipes (dhPK, dhSK) (pK,sK) uID toSend locMsgH locResH




