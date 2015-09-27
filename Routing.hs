{-# LANGUAGE DeriveGeneric #-}
module Routing where

import Crypto hiding (onOrder)
import Class

import FRP.Sodium
import FRP.Sodium.Internal hiding (Event)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Tuple

type Number = Int
type SourceID = KeyHash
type Road = [SourceID]
type Time = POSIXTime
type PipeID = KeyHash


data Request = Request {reqPosition :: Number,
                        reqLength :: Number, -- ^ Total length of the road
                        reqRoad :: Road,  -- ^ Road : list of UserID
                        reqEPK :: RawData,  -- ^ encrypted (Keyhash, PrivKey) for the destination of the road
                        reqTime :: Time,
                        reqPipeKey :: PubKey,
                        reqPipeID  :: PipeID,
                        reqPipeSig :: Signature,
                        reqContent :: RawData}
    deriving Generic
instance SignedClass Request where scHash (Request n l r epk t pK pH s c) = encode (l,r,epk,t,pK,pH,c)
instance IntroClass Request where icPubKey = reqPipeKey
instance Binary Request

data PipePacket = PipePacket {pipeKeyID :: PipeID,
                              pipeSig :: Signature,
                              pipePosition :: Number,
                              pipeDirection :: Bool,
                              pipePayload :: Payload} |
                  PipeClose  {pipeKeyID :: PipeID,
                              pipeSig :: Signature,
                              pipePosition :: Number,
                              pipeDirection :: Bool,
                              pipePayload :: Payload}

    deriving Generic
instance SignedClass PipePacket where scHash (PipePacket kH _ n b m) = encode (kH, n, b, m)
instance Binary PipePacket

type PipeMessage = Either (PipeID, Payload) (PipeID, Payload) --Left on a PipeClose

type RoutingMap = EventEntryMap KeyHash PipePacket
type RoutingMapBhv = BhvTpl RoutingMap
type NewPipe = (Request, Event PipeMessage)

data Routing = Routing {routingLocMap :: RoutingMapBhv,
                        routingRelMap :: RoutingMapBhv,
                        routingNewPipes :: Event NewPipe}

buildRouting :: Event Request -> Event PipePacket -> Reactive Routing
buildRouting reqE packetE = do --On Separe les requetes relayé et locales 
                               (reqRelE, reqLocE) <- splitEvent isLocalRequest reqE 
                               --On construit les cryptoMaps correspondantes
                               (relayMap, _) <- buildCryptoMap reqRelE packetE
                               (localMap, newPipeE) <- buildCryptoMap reqLocE packetE
                               --On bind les actions de relai (et de fermeture des pipes relayés)
                               listenTrans (allEvents $ fst relayMap) $ relayPackets $ Handler $ snd relayMap . M.delete
                               --Retour de la structure de Routing
                               pure $ Routing localMap relayMap  (makeNewPipe <$> newPipeE) 
    where relayPackets :: Handler KeyHash -> PipePacket -> Reactive ()
          relayPackets _ p@(PipePacket _ _ n b _ ) = pure () --TODO : send p{pipePosition = if b then n + 1 else n -1} 
          relayPackets h (PipeClose kID _ _ _ _) = fire h $ kID
          isLocalRequest req = reqPosition req == reqLength req
          makeNewPipe (req, (EventEntry _ e _)) = (req, makePipeMessage <$> e)
          makePipeMessage (PipePacket pID _ _ _ p) = Right (pID,p)
          makePipeMessage (PipeClose  pID _ _ _ p) = Left  (pID,p)


splitEvent :: (e -> Bool) -> Event e -> Reactive (Event e, Event e)
splitEvent f eE = do ((failE, failH), (passE, passH)) <- (,) <$> newEvent <*> newEvent
                     listenTrans eE $ \e -> if f e then passH e else failH e 
                     pure (failE, passE)

instance Binary NominalDiffTime where
        get = fromRational <$> get
        put t = put ((toRational t) :: Rational)





--buildRouting :: Event Request -> Event PipePacket -> (Event PipePacket, Event NewPipe)
--buildRouting reqE packetE = 

{-
data PipeMessage = PipeMessage RawData | PipeClose RawData
    deriving Generic
instance Binary PipeMessage


type NewPipeEventO = (Request, Event PipeMessage)
type NewPipeEvent = Event NewPipeEventO

isPipeExit (PipeExit _ _) = True
isPipeExit _ = False

isPipeData = not . isPipeExit

type Sender a = a -> IO ()
type RelayMap = M.Map PipeID (Event PipeMessage)
type RelayManager = M.Map KeyHash (Behaviour RelayMap) 
type RoutingManager = M.Map KeyHash NewPipeEvent -- Nous est destiné
data RelayOrder = RelayAdd PipeID (Event PipeMessage)
                | RelayDel PipeID

{-| Objectif du module : extraire du flux de pipesData (pour chaque clef) celles qui nous sont destinée et relayer les autres |-}

{-| Seule fonction à utiliser |-}
runRelayManager :: SourceID 
                 -> Handler CryptoOrder -- Commande des order crypto
                 -> Behaviour CryptoMap -- Map des flux de dataPacket par clef
                 -- Maps des (requetes, event de pipes associés) par clef. La première concerne ce qui nous est destiné, la deuxième ce qui est à relayer
                 -> Reactive (Behaviour RoutingManager, Behaviour RelayManager)
runRelayManager me cryptoOrder cMapB = do relayManaRawB <- relayManaRawBR
                                           -- On execute les orders
                                          listenTrans (switchE $ foldr merge never . map snd . M.elems <$> relayManaRawB) id
                                          pure (routingManaB, fmap fst <$> relayManaRawB)
    where 
--          splitDataStream :: Event DataPacket -> (Event Request, Event DataPacket)
--          splitDataStream dataE = swap $ filterEither $ fmap (\x -> case decodeOrFail (signedPayload $ dataContent x) of 
--                                                                Left (_,_,_) -> Left x
--                                                                Right (_,_,r) -> Right r) dataE
          retMap = fmap (extractPipesStream me )  <$> cMapB
          relayManaRawBR :: Reactive (Behaviour (M.Map KeyHash (Behaviour RelayMap, Event (Reactive ())) ))
          (routingManaB, relayManaRawBR) = (fmap fst <$> retMap, swapB $ sequenceA . fmap (makeRelayMap cryptoOrder . snd) <$> retMap)



timeEvent :: Event a -> Event (POSIXTime,a)
timeEvent ev = execute $ fmap f ev
  where f a = (,) <$> ioReactive getPOSIXTime <*> pure a

tag :: (Functor f) => (a -> b) -> f a  -> f (a,b)
tag f c = fmap (\x -> (x,f x)) c

onOrder (RelayAdd pID e) = M.insert pID e
onOrder (RelayDel pID) = M.delete pID



{-| Extrait les flux de pipes d'un flux de datapacket provenant d'un voisin |-}
extractPipesStream :: SourceID 
                   -> Event DataPacket  -- Les requêtes et les PipesMessage provenant d'un voisin
                   -> (Event (Request, Event PipeMessage),
                      Event (Request, Event PipeMessage)) -- 
extractPipesStream me dataE  = (tag extractMsgs myRequests , tag extractMsgs otherRequests)
  where (reqE,pMsgE) = filterEither $ filterJust $ fmap extractDataPkt dataE
        extractDataPkt :: DataPacket -> Maybe (Either Request (KeyHash,PipeMessage))
        extractDataPkt dataPkt = case decodeOrFail (dataSignedPayload dataPkt) of
                                        Right (_,_,req) -> Just $ Left req
                                        otherwise -> case decodeOrFail (dataSignedPayload dataPkt) of 
                                                       Right (_,_,pMsg) -> Just $ Right (datakeyID dataPkt,pMsg)
                                                       otherwise -> Nothing

        validReq :: Event Request -- Requêtes vérifiées
        validReq = snd <$> filterE (\(t,req) -> isRight $ checkRequest me t req) (timeEvent reqE)
        extractMsgs :: Request -> Event PipeMessage -- Retourne le flux vérifié des PipeMessages provenant d'un pipe
        extractMsgs req = snd <$> filterE ((reqPipeID req ==) . fst) pMsgE
        (myRequests,otherRequests) = filterEither $ (\req -> if last (reqRoad req) == me then Left req else Right req) <$> validReq

makeRelayMap ::Handler CryptoOrder -- Commande du cryptomodule
             -> Event (Request, Event PipeMessage) -- Event et pipes associés à relayer provenant d'une source donnée
             -> Reactive (Behaviour RelayMap, -- Map des pipes relayés
                         Event (Reactive ()) ) -- Flux des orders à reactimate (orders d'ajouts successifs)
makeRelayMap fireCrypto newReqE = do (relayOrder,fireOrder) <- newEvent
                                     rMapB <- accum M.empty $ onOrder <$> relayOrder
                                     pure (rMapB, fmap (\(r,d) -> do fireOrder $ RelayAdd (reqPipeID r) d
                                                                     fireCrypto $ CryptoAdd' (reqPipeID r) (reqPipeKey r)) newReqE)

filterEither :: Event (Either a b) -> (Event a, Event b)
filterEither ev = (fromLeft <$> filterE isLeft ev,
                   fromRight <$> filterE isRight ev)
     where fromLeft (Left x) = x
           fromRight (Right x) =x 

checkRequest me t req@(Request n l r epk t' pK pH s c)
    | l > roadLengthMax                 = Left "Rejected road : too long"
    | n > l                             = Left "Incorrect RequestPosition"
    | l /= length r                      = Left "Incorrect RoadLength"
    |  t - t' > maxDelay               = Left "Obsolete Request" 
    | r !! n /= me                       = Left "Not destinated to me"
    | computeHashFromKey pK /= pH        = Left "Invalid tuple KeyHash/PubKey"
    | checkSig pK s $ requestHash req   = Right ()
    | otherwise                         = Left "Invalid signature"


decodePipeMessage :: Payload -> Either String (PipeMessage,Number)
decodePipeMessage (Payload s us) = case (decodeOrFail s, decodeOrFail us) of
                                    (Right (_,_,msg), Right (_,_,pos)) -> Right (msg,pos)
                                    (Left (_,_,e), _) -> Left e
                                    (_, Left (_,_,e)) -> Left e

-}
