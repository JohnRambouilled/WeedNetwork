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
data WeedOrder = WeedLog Log |
                 WeedCreateTimer TimerEntry Time IOAction |
                 WeedPerformIO IOAction
type IOAction = IO ()

type Sender = RawData -> WeedMonad ()

data Client = Client { clUserID :: UserID,
                       clKeyPair :: KeyPair,
                       clTime :: TVar Time, -- Should be updated before each call... 
                       clSender :: Sender,
                       clRelayedPipes :: TVar RelayedPipesMap,
                       clLocalPipes :: TVar LocalPipeMap,
                       clNeighbours :: TVar NeighboursMap,
                       clRessources :: TVar RessourcesMap,
                       clDestinaries :: TVar DestinariesMap,
                       clProtocols :: TVar ProtocolMap}


data Log = Log { logModule :: String,
                 logFunction :: String,
                 logStatus :: LogStatus,
                 logMessage :: String}

data LogStatus = InvalidPacket | Normal | Fail | Error


