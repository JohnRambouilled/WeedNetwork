{-# LANGUAGE DeriveGeneric #-}
module Communication where

import Crypto
import Routing

import Data.ByteString.Lazy hiding (split,last)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Banana.Switch
import Control.Event.Handler
import GHC.Generics


--data ComMessage = ComInit {cmID :: ComID, cmPayload :: RawData}
--                | ComData {cmID :: ComID, cmPayload :: RawData}
--                | ComExit {cmID :: ComID, cmPayload :: RawData}

data ComInit    = ComInit {ciID :: ComID, ciPayload :: RawData}
        deriving Generic
instance Binary ComInit
data ComMessage = ComData {cmID :: ComID, cmPayload :: RawData} 
                | ComExit {cmID :: ComID, cmPayload :: RawData} 
     deriving Generic
instance Binary ComMessage

type ComPacket = Either ComInit ComMessage
type ComID = Int


data ComOrders = ComAdd ComID ComEntry
               | ComDelete ComID

type ComMap = M.Map ComID ComEntry

type ComEntry = Handler ComMessage



buildCommunication :: Frameworks t => Handler ComInit -> Event t ComOrders -> Event t ComPacket -> Moment t ()
buildCommunication comIH comOE comPE = let (comIE, comME) = split comPE
                                           comBHV = accumB M.empty (onComOrder <$> comOE) in
                                       do reactimate (comIH <$> comIE)
                                          reactimate $ apply (onComMessage <$> comBHV) comME



onComOrder :: ComOrders -> ComMap -> ComMap
onComOrder (ComAdd cID cE) = M.insert cID cE
onComOrder (ComDelete cID) = M.delete cID

onComMessage :: ComMap -> Handler ComMessage
onComMessage cMap cm = case cmID cm `M.lookup` cMap of
                        Nothing -> pure ()
                        Just cE -> cE $ cm


