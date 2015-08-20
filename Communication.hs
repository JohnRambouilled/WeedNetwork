{-# LANGUAGE DeriveGeneric #-}


{- TODO Gérer les ComExit -}
module Communication where

import Sources hiding (onOrder)
import Pipes hiding (onOrder)

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

instance Binary ComMessage

data ComOrder = ComAdd ComID (Event ComMessage)
              | ComDel ComID


-- Communications provenant d'une seule et unique source
type CommunicationEvent = Event (ComID, Event ComMessage) -- Event des nouvelles communications

{-| Associe à chaque sourceID le flux permettant d'écouter les nouvelles communications. |-}
type ComMap = M.Map SourceID CommunicationEvent

type ComManager = M.Map ComID (Event ComMessage)

data SourceManagerEntry = SourceManagerEntry { smeComManager :: Behaviour ComManager,
                                               smeFireOrder  :: ComOrder -> Reactive (),
                                               smeFiredStream :: Event (Reactive ()) }
type SourceManager = M.Map SourceID SourceManagerEntry

{-| Unique fonction utile : prend les events de sources et traite les comIDs |-}
runCommunicationManager :: Behaviour SourceMap -> Reactive (Behaviour SourceManager)
runCommunicationManager sMapB = do sManaB <- newCommunicationManager sMapB
                                   -- On reactimate le dernier event d'order fired
                                   listenTrans (handleSourceManager sManaB) id -- TODO ATTENTION normalement on n'a pas besoin de l'unregister
                                   pure sManaB




onOrder (ComAdd cID val) = M.insert cID val
onOrder (ComDel cID) = M.delete cID


onNewComID :: CommunicationEvent -- Flux des nouvelles communications de la source considérée
           -> Behaviour ComManager -- ComManager actuel
           -> (ComOrder -> Reactive ()) -- Fire des ComOrders
           -> Event (Reactive ()) -- Events des fires successifs ajoutant des comID à la behaviour
onNewComID comE cManaB fireOrder = filterJust $ snapshot f comE cManaB
  where f :: (ComID, Event ComMessage) -> ComManager -> Maybe (Reactive ())
        f (cID, cMsgE) cMana = case cID `M.lookup` cMana of
                                 Just _ -> Nothing
                                 Nothing -> Just $ fireOrder $ ComAdd cID cMsgE



{-| À chaque nouveau comInit, on renvoie l'event contenant les comMessages y appartenant.
    ATTENTION : aucune mémoire des comInit, il se peut que le même soit reçu plusieurs
    fois sans avoir été close. |-}
newCommunicationModule :: Event ComMessage -- ComMessages provenant d'une même source (actualisé en fonction de la pipesMap)
                       -> CommunicationEvent -- Event des provenant d'un comID d'une source donnée (fired à chaque nouveau ComInit) (actualisé en fonction de la pipeMap)
newCommunicationModule cMsgE = filterJust $ onComMsg <$> cMsgE --filterJust $ f <$> cMsgE
  where onComMsg :: ComMessage -> Maybe (ComID, Event ComMessage)
        onComMsg (ComInit cID _) = Just (cID, cIDevents cID )
        onComMsg _ = Nothing
        cIDevents :: ComID -> Event ComMessage
        cIDevents cID = filterE ((cID==) . cmComID) cMsgE

filterDecode :: (Binary a) => Event RawData -> Event a
filterDecode rawE = extractData <$> filterE isRight (decodeOrFail <$> rawE)
  where extractData (Right (_,_,r)) = r


{-| Retourne la map actualisée qui à chaque source associe 
        - la map des comID 
        - Un moyen d'envoyer des orders
        - des orders fired à reactimate
|-}
newCommunicationManager :: Behaviour SourceMap -> Reactive (Behaviour SourceManager)
newCommunicationManager sMapB = hold M.empty $ execute $ value srcManager
    where comMsgStream :: PipeMessageEvent -> Event ComMessage
          comMsgStream pMsgE = filterDecode $ messageContent . snd <$> pMsgE
          cMap :: Behaviour ComMap
          cMap = fmap (newCommunicationModule . comMsgStream) <$>  sMapB
          srcManager :: Behaviour (Reactive SourceManager)
          srcManager = sequenceA . fmap newCommunicationManager' <$> cMap

newCommunicationManager' :: CommunicationEvent -> Reactive SourceManagerEntry 
newCommunicationManager' cMsgE = do (cOrders,fireOrder) <- newEvent
                                    cManaB <- accum M.empty $ onOrder <$> cOrders
                                    pure $ SourceManagerEntry cManaB fireOrder (onNewComID cMsgE cManaB fireOrder) 



{-| Retourne le flux actuel des events à reactimate |-}
handleSourceManager :: Behaviour SourceManager -> Event (Reactive ())
handleSourceManager sManaB = switchE $ foldr merge never . fmap smeFiredStream . M.elems <$> sManaB



