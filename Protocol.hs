{-# LANGUAGE DeriveGeneric #-}
module Protocol where

import Sources
import Pipes
import Communication

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


type ProtoMap = M.Map ProtocolID (RawData -> ProtoCallback) -- Map constante des protocoles supportés
type ProtoManager = M.Map SourceID (Event (IO ()))


{-| Seule fonction à utiliser. |-}
runProtocolManager :: ProtoMap -- Descriptif des protocoles supportés
                   -> Behaviour SourceManager -- Map des communications pour chaque source
                   -> Reactive (Behaviour ProtoManager) -- Map des actions de chaque source (pour chaque protocole)
runProtocolManager pMap sManaB = do pManaB <- swapB $ ret <$> sManaB
                                    -- Execute l'event actuel dans la pipeMap
                                    listen (handleProtocolManager pManaB) id
                                    pure pManaB
  where ret :: SourceManager -> Reactive ProtoManager
        ret sMana = sequenceA $ newProtocolManager pMap . smeComManager <$> sMana

onProtoInit' :: ProtoMap 
             ->  Event ProtoMessage -- Stream des messages provenant d'un comID
             -> ProtoMessage -- ProtoInit provenant du même comID
             -> Maybe (Event (IO ())) -- Stream des actions à effectuer (fired à chaque reception de payload)
                                    -- Retourne nothing s'il n'a pas reçu de ProtoInit ou si le protocole n'est pas supporté
onProtoInit' pMap protoMsgE (ProtoInit protoID raw) = case protoID `M.lookup` pMap of
                                                       Nothing -> Nothing
                                                       Just cb ->  Just $ fmap ((cb raw $ ) . protocolPayload) protoMsgE
onProtoInit' _ _ _ = Nothing

onProtoInit :: ProtoMap 
            -> Event ProtoMessage  -- Tous les protoMessage d'un comID
            -> Event (Event (IO ())) -- Stream des prochaines actions à effectuer (à chaque protoInit)
onProtoInit pMap protoMsgE = filterJust $ onProtoInit' pMap protoMsgE <$> protoMsgE



extractProtocol:: ProtoMap 
               -> Event ComMessage -- Occurence d'un nouveau comInit
               -> Reactive (Event (IO ())) -- Stream des traitements à faire pour le DERNIER protoinit reçu (à chaque protoInit)
extractProtocol pMap cMsgE = switchE <$> hold never (onProtoInit pMap $ protoMsgE cMsgE)
       where protoMsgE :: Event ComMessage -> Event ProtoMessage
             protoMsgE cMsgE = filterDecode $ cmPayload <$> cMsgE

{-| Génère, pour chaque comID, le flux des actions à effectuer (selon le dernier protoInit reçu) |-}
newProtocolManager' :: ProtoMap -> Behaviour ComManager -> Reactive (Behaviour (M.Map ComID (Event (IO ()))))
newProtocolManager' pMap cManaB = swapB ret
    where ret :: Behaviour (Reactive (M.Map ComID  (Event (IO ()))))
          ret = sequenceA . fmap (extractProtocol pMap) <$> cManaB

{-| Génère pour un comManager donné, l'union de toutes les actions de protocoles à effectuer (indépendamment du comManager) |-}
newProtocolManager :: ProtoMap -> Behaviour ComManager -> Reactive (Event (IO ()))
newProtocolManager pMap cManaB = switchE <$> ret
    where ret :: Reactive (Behaviour (Event (IO ())))
          ret = do comProtoB <- newProtocolManager' pMap cManaB
                   pure $ foldr merge never . M.elems <$> comProtoB




handleProtocolManager :: Behaviour ProtoManager -> Event (IO ())
handleProtocolManager pManaB = switchE ret
  where ret :: Behaviour (Event (IO ()))
        ret = foldr merge never . M.elems <$> pManaB


swapB :: (Ord k, Eq k) => Behaviour (Reactive (M.Map k a)) -> Reactive (Behaviour (M.Map k a))
swapB mapB = hold M.empty $ execute $ value mapB




{-
createProtocolManager :: ProtoMap -> Behaviour SourceManager -> Behaviour ProtoManager
createProtocolManager pMap sManaB = pure M.empty
  where -- Pour chaque source, à chaque nouvelle communication, fire l'event des actions à faire du dernier protoInit reçu
        protoEvents :: ComMap -> M.Map SourceID (Event (Event (IO () )))
        protoEvents cMap =  execute . fmap (newProtocolManager pMap) . fmap snd <$> cMap
        -- Pour chaque source, le comportement par défaut consiste à executer le protocole de chaque comInit
        execProto :: Event (Event (IO ()))  -- à chaque nouvelle communication, ce qu'il faut faire
                   -> Reactive (Event (IO ())) -- Fusionne les events des nouveaux protoIDs et ne conserve que la derniere version
        execProto newComE = switchE <$> accum never (fmap merge newComE)




{-| Listen le callback à partir du protocol ID trouvé dans la map
    Retourne une fonction pour unregister le callback de protocole actuellement utilisé |-}
createProtocolManager :: ProtoMap -> (ComID, Event ComMessage) -> Reactive (Event (IO ())) --Event (IO ())
createProtocolManager pMap (comID,cMsgEB) = do 
         -- Les actions à effectuer du protocole courant (d'après le dernier protoInit reçu)
         ret <- (hold never $ newProtocolManager pMap cMsgEB :: Reactive (Behaviour (Event (IO ()))))

         pure $ switchE ret
                                                --reactimate $ switchE $ hold never (newProtocolManager pMap cMsgEB)
            -- execute $ fmap reactimate $ newProtocolManager pMap cMsgEB --newProtocolManager pMap cMsgEB 
-}

{-
onProtoInit :: ProtoMap -> (ComID,ProtoMessage) -> ProtoManager -> ProtoManager
onProtoInit protoMap (cID,(ProtoInit protoID initMsg )) pManager = case protoID `M.lookup` protoMap of
                                                                       Nothing -> pManager
                                                                       Just cb -> M.insert cID (cb initMsg) pManager



newProtoManager' :: ProtoMap -> Event (ComID,ComMessage) -> Behaviour ProtoManager -> Event (IO ())
newProtoManager' protoMap cMsgE pManagerB = filterJust $ snapshot call cMsgE pManagerB
  where call (cID,cMsg) pManager = case cID `M.lookup` pManager of
                                    Nothing -> Nothing
                                    Just cb -> Just (cb $ cmPayload cMsg)

newProtoManager :: (ComID, Behaviour (Event ComMessage))  -> Behaviour ProtoManager -> Behaviour  (Event (IO ()))
newProtoManager (cID, cMsgEB) pManaB = pure never
    where   ret :: Behaviour (Event (ComID,ComMessage))
            ret = fmap tagComID <$> cMsgEB  
            tagComID x = (cID,x)

-}
