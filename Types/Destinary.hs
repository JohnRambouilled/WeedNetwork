module Types.Destinary where
import Types.Callbacks
import Types.Neighbour
import Types.Routing
import Types.Communication
import Control.Concurrent.STM
import qualified Data.Map as M


data DestinaryID = DestinaryID
data DestinaryError = DestinaryError

data DestinaryEntry = DestinaryEntry {destinaryPipes :: [PipeID],
                                      destinaryTimer :: TimerEntry,
                                      destinaryCallback :: Callback DestinaryError PipeMessage, -- Commun à tous les pipes associés à la source
                                      destinaryComID :: TVar ComModule}
newtype DestinaryModule = DestinaryModule {destinaryControlMap :: M.Map DestinaryID DestinaryEntry}


-- ajoute les entrées au routingmodule à chaque requete, et maintient une map des sources
--newDestinaryModule :: TVar RoutingModule -> TChan Request -> IO (TVar DestinaryModule)


--private
--Transforme le Callback fournis par un ComModule pour le fournir a routing. Gere le pipeClose au passage en retenant le pipeID
--genDestinaryCallback :: Callback DestinaryError PipeMessage -> Request -> Callback RoutingError PipeMessage
--

