{-# LANGUAGE DeriveGeneric #-}

{- TODO Gérer les ComExit -}
module Communication where

import Sources hiding (onOrder)
import Pipes hiding (onOrder)
import Crypto hiding (onOrder)
import Routing hiding (onOrder)

import FRP.Sodium
import FRP.Sodium.Internal hiding (Event)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Data.List


type ComID = Int

data ComMessage = ComInit {cmComID :: ComID, cmPayload :: RawData}
                | ComData {cmComID :: ComID, cmPayload :: RawData}
                | ComExit {cmComID :: ComID, cmPayload :: RawData}
             deriving Generic
isComInit (ComInit _ _) = True
isComInit _ = False
instance Binary ComMessage

data ComOrder = ComAdd ComID (Event ComMessage)
              | ComDel ComID


-- Communications provenant d'une seule et unique source
type NewComEvent = Event (ComID, Event ComMessage) -- Event des nouvelles communications

{-| Associe à chaque sourceID le flux permettant d'écouter les nouvelles communications. |-}

type ComMap = M.Map ComID (Event ComMessage)

data ComManagerEntry = ComManagerEntry { cmeComManager :: Behaviour ComMap,
                                         cmeFireOrder  :: ComOrder -> Reactive (),
                                         cmeFiredStream :: Event (Reactive ()) }
type ComManager = M.Map SourceID ComManagerEntry

runComManager :: Behaviour PipeManager -> Reactive (Behaviour ComManager)
runComManager pManaB = do cManaB <- newComManager pManaB
                          -- Listen des orders
                          listenTrans (handleComManager cManaB) id -- TODO attention, n'est censé être appelé qu'une fois
                          pure cManaB

onOrder (ComAdd cID val) = M.insert cID val
onOrder (ComDel cID) = M.delete cID


onNewComID :: NewComEvent -- Flux des nouvelles communications de la source considérée
           -> Behaviour ComMap -- ComManager actuel
           -> Handler ComOrder -- Fire des ComOrders
           -> Event (Reactive ()) -- Events des fires successifs ajoutant des comID à la behaviour
onNewComID comE cManaB fireOrder = filterJust $ snapshot f comE cManaB
  where f :: (ComID, Event ComMessage) -> ComMap -> Maybe (Reactive ())
        f (cID, cMsgE) cMana = case cID `M.lookup` cMana of
                                 Just _ -> Nothing
                                 Nothing -> Just $ fireOrder $ ComAdd cID cMsgE

{-| Transforme le flux de pipes message en flux de nouvelles communications |-}
extractNewCom :: Event PipeMessage -> NewComEvent
extractNewCom pMsgE = fmap ((,) <$> cmComID <*> extractCommunication)  cInitE
  where cMsgE :: Event ComMessage
        cMsgE = filterDecode $ messageContent <$> pMsgE
        (cInitE,cDataE) = filterEither $ fmap (\x -> if isComInit x then Left x else Right x) cMsgE
        extractCommunication :: ComMessage -> Event ComMessage
        extractCommunication (ComInit cID _) = filterE ((== cID) . cmComID) cDataE
        extractCommunication _ = error "Extract communication has been called with a wrong type."


{-| Retourne la map actualisée qui à chaque source associe 
        - la map des comID 
        - Un moyen d'envoyer des orders
        - des orders fired à reactimate
|-}
newComManager :: Behaviour PipeManager -> Reactive (Behaviour ComManager)
newComManager pManaB = swapB $ fmap sequenceA cMapB 
    where 
          cMapB :: Behaviour (M.Map SourceID (Reactive ComManagerEntry))
          cMapB = fmap (newComMap . extractNewCom) <$>  pipesStream pManaB


newComMap :: NewComEvent -> Reactive ComManagerEntry 
newComMap cMsgE = do (cOrders,fireOrder) <- newEvent
                     cManaB <- accum M.empty $ onOrder <$> cOrders
                     pure $ ComManagerEntry cManaB fireOrder (onNewComID cMsgE cManaB fireOrder) 

{-| Retourne le flux actuel des events à reactimate |-}
handleComManager :: Behaviour ComManager -> Event (Reactive ())
handleComManager sManaB = switchE $ foldr merge never . fmap cmeFiredStream . M.elems <$> sManaB


