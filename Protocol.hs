{-# LANGUAGE DeriveGeneric #-}
module Protocol where

import Crypto
--import Sources
import Pipes
import Communication
import Routing

import FRP.Sodium
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Data.List
import Data.Maybe


type ProtocolID = Int

data ProtoMessage = ProtoData {protocolID :: ProtocolID, protocolPayload :: RawData}
                  | ProtoInit {protocolID :: ProtocolID, protocolPayload :: RawData}
       deriving Generic

instance Binary ProtoMessage
                  

type ProtoCallback = RawData -> IO ()


type ProtoConfMap = M.Map ProtocolID (RawData -> ProtoCallback) -- Map constante des protocoles supportés
type ProtoMap = M.Map ComID (Event (IO ()))
type ProtoManager = M.Map SourceID (Behaviour ProtoMap)

{-| Seule fonction à utiliser qui, à chaque source, à chaque comID associe le flux d'évènement du DERNIER protoInit reçu |-}
runProtoManager :: ProtoConfMap -> Behaviour ComManager -> Reactive (Behaviour ProtoManager)
runProtoManager conf cManaB = do pManaB <- makeProtoManager conf cManaB
                                 -- On execute le flux d'évenements présent à chaque instant dans la map
                                 listen (execProtoManager pManaB) id --TODO attention, ne devrait pas être unregister
                                 pure pManaB

onProtoInit' :: ProtoConfMap 
             ->  Event ProtoMessage -- Stream des messages provenant d'un comID
             -> ProtoMessage -- ProtoInit provenant du même comID
             -> Maybe (Event (IO ())) -- Stream des actions à effectuer (fired à chaque reception de payload)
                                    -- Retourne nothing s'il n'a pas reçu de ProtoInit ou si le protocole n'est pas supporté
onProtoInit' conf protoMsgE (ProtoInit protoID raw) = case protoID `M.lookup` conf of
                                                       Nothing -> Nothing
                                                       Just cb ->  Just $ fmap ((cb raw $ ) . protocolPayload) protoMsgE
onProtoInit' _ _ _ = Nothing

onProtoInit :: ProtoConfMap 
            -> Event ProtoMessage  -- Tous les protoMessage d'un comID
            -> Event (Event (IO ())) -- Stream des prochaines actions à effectuer (à chaque protoInit)
onProtoInit conf protoMsgE = filterJust $ onProtoInit' conf protoMsgE <$> protoMsgE


extractProtocol:: ProtoConfMap 
               -> Event ComMessage -- Occurence d'un nouveau comInit
               -> Reactive (Event (IO ())) -- Stream des traitements à faire pour le DERNIER protoinit reçu (à chaque protoInit)
extractProtocol conf cMsgE = switchE <$> hold never (onProtoInit conf $ protoMsgE)
       where protoMsgE :: Event ProtoMessage
             protoMsgE = filterDecode $ cmPayload <$> cMsgE

makeProtoMap :: ProtoConfMap -> Behaviour ComMap -> Reactive (Behaviour ProtoMap)
makeProtoMap conf cMapB = swapB $ sequenceA . fmap (extractProtocol conf) <$> cMapB
makeProtoManager :: ProtoConfMap -> Behaviour ComManager -> Reactive (Behaviour ProtoManager)
makeProtoManager conf cManaB = swapB $ sequenceA . fmap (makeProtoMap conf . cmeComManager) <$> cManaB


execProtoManager :: Behaviour ProtoManager -> Event (IO ())
execProtoManager pManaB = switchE $ foldr merge never . M.elems . fmap execProtoMapB <$> pManaB
  where execProtoMapB :: Behaviour ProtoMap -> Event (IO ())
        execProtoMapB pMapB = switchE $ foldr merge never . M.elems <$> pMapB

