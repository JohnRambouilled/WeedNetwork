module Types.Routing where

import Types.Callbacks
import Types.Neighbour
import Control.Concurrent.STM
import qualified Data.Map as M

newtype PipeID = PipeID Int
newtype SourceID = SourceID Int
data RoutingError = RoutingError
data PipeMessage = PipeMessage

data RoutingEntry = RoutingEntry { routingPubKey :: PubKey,
                                   routingCallback :: Callback RoutingError PipeMessage,
                                   routingTimer :: TimerEntry,
                                   routingNodes :: (NeighID, NeighID)}

-- Contiens tout les pipes (relayés, leech et seed)
newtype RoutingModule = RoutingModule {routingControlMap :: M.Map PipeID RoutingEntry}

--private

--public
-- appelle le callback sur les pipemessage après avoir vérifié la conformité du pipepacket associé
-- gère aussi les neighbreak et appelle le callback des pipes associés avec une routingerror
--newRoutingModule :: TChan PipePacket -> TChan NeighBreak -> IO (TVar RoutingModule)

--enregistre une règle de routing pour un pipe donné, et retourne le timer à enregistrer
--registerRoutingEntry :: TVar RoutingModule -> PipeID -> PubKey -> Callback RoutingError PipeMessage -> STM (IO ())
--

--utilitaire
-- répartit les requetes entrantes en (relay, seed, leech) après avoir vérifié leur conformité
--splitRequest :: TChan Request -> IO (TChan Request, TChan Request, TChan Request)
--
--
------- RELAY
--


-- Recoit une requete à relayer, ajoute la routingentry à routing (en lancant le timer)
--onRelayRequest :: TVar RoutingModule -> Request -> IO ()
--


-- Ressources :
--
data RessourcePolicy = RessourcePolicy
data RessourceEntry = RessourceEntry {ressourceSources :: [SourceID],
                                      ressourcePolicy :: RessourcePolicy}
data RessourceGraph = RessourceGraph 




