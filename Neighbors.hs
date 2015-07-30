{-# LANGUAGE DeriveGeneric #-}
module Neighbors where

import Crypto

import Data.ByteString hiding (split)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Banana.Switch
import Control.Event.Handler

import GHC.Generics


data NeighHello = NeighHello RawData
    deriving Generic
data NeighData = NeighData RawData |
                 NeighClose RawData
    deriving Generic


buildNeighbors :: Handler Payload -> Handler CryptoOrders -> Handler CryptoNewKey
buildNeighbors payH cOrderH (CryptoNewKey i kH addH) = case decodeOrFail (signedPayload $ introContent i) of
                                                                Right (_,_, NeighHello d) -> void $ register addH $ manageData
                                                                Left (_,_,e) -> print e
    where manageData (DataPacket _ _ pay) = case decodeOrFail $ signedPayload pay of
                                        Left (_,_,e) -> print e
                                        Right (_,_,NeighClose d) -> cOrderH $ CryptoDelete kH
                                        Right (_,_,NeighData d)  -> payH pay

instance Binary NeighHello
instance Binary NeighData
