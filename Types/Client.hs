{-# LANGUAGE DeriveGeneric #-}
module Types.Client where

import Types.Neighbours
import Types.Pipes
import Types.Timer
import Types.Packets
import Types.Crypto
import Types.Random
import Types.Destinaries
import Types.Ressources
import Types.Protocol
import Types.Graph.RoadGraph
import Packets

import Control.Concurrent.STM (TVar)
import Control.Monad.STM
import Control.Monad.Writer
import Control.Monad.Reader
import GHC.Generics

newtype WeedMonad a = WeedMonad {runWeedMonad :: WriterT WeedOrders (ReaderT Client STM) a}  -- One monad to rule them all... Keep it simple?
              deriving Generic

newtype WeedOrders = WeedOrders {runWeedOrders :: [WeedOrder] }
              deriving Generic
data WeedOrder = WeedLog Log |
                 WeedPerformIO IOAction
type IOAction = IO ()

type Sender = RawData -> WeedMonad ()

data Client = Client { clUserID :: UserID,
                       clKeyPair :: KeyPair,
                       clSender :: Sender,
                       clLogHandler :: Log -> IO (),
                       clTime :: TVar Time, -- Should be updated before each call... 
                       clRndGen :: TVar StdGen,
                       clGraph :: TVar RoadGraph,
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

instance Show LogStatus where show InvalidPacket = " # Invalid Packet # -> "
                              show Normal =        " # Normal         # -> "
                              show Fail =          " # Fail           # -> "
                              show Error =         " # Error          # -> "

instance Show Log where show (Log mod fun stat mes) = show stat ++ "[" ++ mod ++ ":" ++ fun ++ "]  >> " ++ mes

                        
instance Functor WeedMonad     where fmap f (WeedMonad a) = WeedMonad (fmap f a)
instance Applicative WeedMonad where pure = WeedMonad . pure
                                     WeedMonad f <*> WeedMonad a = WeedMonad (f <*> a)
instance Monad WeedMonad       where (WeedMonad a) >>= f = WeedMonad $ a >>= runWeedMonad . f
instance Monoid WeedOrders     where mempty = WeedOrders mempty
                                     mappend (WeedOrders l1) (WeedOrders l2) = WeedOrders (mappend l1 l2)
                       
