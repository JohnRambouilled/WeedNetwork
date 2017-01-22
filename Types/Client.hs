module Types.Client where

import Types.Neighbours
import Types.Pipes
import Types.Timer
import Types.Packets
import Types.Crypto
import Types.Destinaries
import Types.Ressources
import Types.Protocol
import Packets

import Control.Concurrent.STM (TVar)
import Control.Monad.STM
import Control.Monad.Writer
import Control.Monad.Reader

type WeedMonad = WriterT WeedOrders (ReaderT Client STM)  -- One monad to rule them all... Keep it simple?

type WeedOrders = [WeedOrder]
data WeedOrder = WeedLog String |
                 WeedCreateTimer TimerEntry Time IOAction |
                 WeedPerformIO IOAction
type IOAction = IO ()


data Client = Client { clUserID :: UserID,
                       clKeyPair :: KeyPair,
                       clTime :: Time, -- Should be updated before each call... 
                       clRelayedPipes :: TVar RelayedPipesMap,
                       clIncomingPipes :: TVar IncomingPipesMap,
                       clOutgoingPipes :: TVar OutgoingPipesMap,
                       clNeighbours :: TVar NeighboursMap,
                       clRessources :: TVar RessourcesMap,
                       clDestinaries :: TVar DestinariesMap,
                       clProtocols :: TVar ProtocolMap}





