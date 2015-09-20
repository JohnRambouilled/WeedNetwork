{-# LANGUAGE DeriveGeneric #-}

{- TODO Gérer les ComExit -}
module Communication where

import Class
--import Sources hiding (onOrder)
import Pipes 
import Crypto 
import Routing

import FRP.Sodium
import FRP.Sodium.Internal hiding (Event)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Data.List


type ComID = Int

data ComInit = ComInit {ciComID :: ComID, ciPayload :: RawData}
             deriving Generic
data ComMessage = ComData {cmComID :: ComID, cmPayload :: RawData}
                | ComExit {cmComID :: ComID, cmPayload :: RawData}
             deriving Generic
isComExit (ComExit _ _) = True
isComExit _ = False
instance Binary ComMessage
instance Binary ComInit

data ComOrder = ComAdd ComID (Event ComMessage)
              | ComDel ComID


-- Communications provenant d'une seule et unique source
type NewComEvent = Event (ComID, Event ComMessage) -- Event des nouvelles communications

{-| Associe à chaque sourceID le flux permettant d'écouter les nouvelles communications. |-}

type ComMap = EventEntryMap ComID ComMessage


{-| Transforme le flux de pipes message en flux de nouvelles communications |-}

newComMap :: Event ComInit -> Event ComMessage -> Reactive (BhvTpl ComMap, Event (Reactive ()))
newComMap ciE cmE = do (cMapB,order) <- newBhvTpl M.empty
                       pure ((cMapB,order), 
                              toListen order cMapB)
  where   onNewComID :: Modifier ComMap -> ComInit -> ComMap -> Maybe (Reactive ())
          onNewComID order cInit cMap = case ciComID cInit `M.lookup` cMap of
                                            Just _ -> Nothing
                                            Nothing -> Just $ do e <- newEventEntry $ (ciComID cInit ==) . cmComID
                                                                 order $ M.insert (ciComID cInit) e

          onExit (ComExit cID _) = M.delete cID
          onExit _ = error "onExit has not been called with a ComExit..."
          toListen :: Modifier ComMap -> Behaviour ComMap -> Event (Reactive ())
          toListen order cMapB = merge (newComE order cMapB) (decoE order)
          newComE order cMapB = filterJust $ snapshot (onNewComID order) ciE cMapB
          decoE order = order . onExit <$> filterE isComExit cmE


splitComStream :: Event RawData -> Event (Either ComInit ComMessage)
splitComStream rDataE = filterJust $ decodeStream <$> rDataE
  where decodeStream :: RawData -> Maybe (Either ComInit ComMessage)
        decodeStream raw = case decodeOrFail raw of
                             Right (_,_,cI) -> Just $ Left cI
                             Left _ -> case decodeOrFail raw of
                                         Right (_,_,cM) -> Just $ Right cM
                                         Left _ -> Nothing

type ComManager = M.Map SourceID (BhvTpl ComMap)
buildComManager :: Behaviour DataManager -> Reactive (Behaviour ComManager)
buildComManager dataManaB = do ret <- swapBRM $ fmap f <$> dataManaB
                               let comManaB = fmap fst <$> ret
                                   listenMap = fmap snd <$> ret
                               -- listening every orders
                               listenTrans (allEvents listenMap) id
                               pure comManaB
  where f :: Event RawData -> Reactive (BhvTpl ComMap, Event (Reactive ()))
        f rDataE = newComMap (cInitStream rDataE) (cMsgStream rDataE)
        cInitStream :: Event RawData -> Event ComInit 
        cInitStream rDataE = fromLeft <$> filterE isLeft (splitComStream rDataE)
        cMsgStream rDataE = fromRight <$> filterE isRight (splitComStream rDataE)
