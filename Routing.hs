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
                        reqEPK :: DHPubKey,  -- ^ encrypted (Keyhash, PrivKey) for the destination of the road
                        reqTime :: Time,
                        reqPipeKey :: PubKey,
                        reqPipeID  :: PipeID,
                        reqPipeSig :: Signature,
                        reqContent :: RawData}
    deriving Generic
instance SignedClass Request where scHash (Request n l r epk t pK pH s c) = encode (l,r,epk,t,pK,pH,c)
                                   scKeyHash = reqPipeID
                                   scSignature = reqPipeSig
                                   scPushSignature r s = r{reqPipeSig = s}
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
                                      scKeyHash = pipeKeyID
                                      scSignature = pipeSig
                                      scPushSignature p s = p{pipeSig = s}
instance Binary PipePacket

type PipeMessage = Either (PipeID, Payload) (PipeID, Payload) --Left on a PipeClose

type RoutingMap = EventEntryMap KeyHash PipePacket
type RoutingMapBhv = BhvTpl RoutingMap
data NewPipe = NewPipe {npRoad :: Road,
                        npSource :: SourceID,
                        npPubKey :: PubKey,
                        npSender :: PipeMessage -> PipePacket,
                        npPipeID :: PipeID,
                        npTime :: Time,
                        npContent :: RawData,
                        npMessageEvent :: Event PipeMessage}

requestToNewPipe :: KeyPair -> Request -> Event PipeMessage -> Maybe NewPipe
requestToNewPipe (upK,uk) (Request n _ r epk t pK pID _ cnt) e = (\s -> NewPipe r (head r) pK s pID t cnt e) <$> sender
            where sender = pipeMessageToPipePacket n (n == 0) <$> decryptPrivKey epk uk

pipeMessageToPipePacket :: Number -> Bool -> KeyPair -> PipeMessage -> PipePacket
pipeMessageToPipePacket n b pK (Left  (pID,d)) = sign pK $ PipeClose  pID emptySignature n b d 
pipeMessageToPipePacket n b pK (Right (pID,d)) = sign pK $ PipePacket pID emptySignature n b d 




data Routing = Routing {routingLocMap :: RoutingMapBhv,
                        routingRelMap :: RoutingMapBhv,
                        routingNewPipes :: Event NewPipe,
                        routingRelayedPackets :: Event PipePacket,
                        routingRequestHandler :: Handler Request,
                        routingPacketHandler :: Handler PipePacket}

buildRouting :: KeyPair -> Reactive Routing
buildRouting uK = do (reqE, reqH) <- newEvent'
                     (packetE, packetH) <- newEvent'
                     --On Separe les requetes relayé et locales 
                     (reqRelE, reqLocE) <- splitEvent isLocalRequest reqE 
                     --Event des messages relayés
                     (relPE, relPH) <- newEvent
                     --On construit les cryptoMaps correspondantes
                     (relayMap, _) <- buildCryptoMap reqRelE packetE
                     (localMap, newPipeE) <- buildCryptoMap reqLocE packetE
                     --On bind les actions de relai (et de fermeture des pipes relayés)
                     listenTrans (allEvents $ fst relayMap) $ relayPackets relPH $ Handler $ snd relayMap . M.delete
                     --Retour de la structure de Routing
                     pure $ Routing localMap relayMap  (filterJust (makeNewPipe <$> newPipeE)) relPE reqH packetH
    where relayPackets :: (PipePacket -> Reactive ()) -> Handler KeyHash -> PipePacket -> Reactive ()
          relayPackets h _ p@(PipePacket _ _ n b _ ) = h p{pipePosition = if b then n + 1 else n -1} 
          relayPackets _ h (PipeClose kID _ _ _ _) = fire h $ kID
          isLocalRequest req = reqPosition req == reqLength req
          makeNewPipe (req, (EventEntry _ e _)) = requestToNewPipe uK req $ makePipeMessage <$> e
          makePipeMessage (PipePacket pID _ _ _ p) = Right (pID,p)
          makePipeMessage (PipeClose  pID _ _ _ p) = Left  (pID,p)


splitEvent :: (e -> Bool) -> Event e -> Reactive (Event e, Event e)
splitEvent f eE = do ((failE, failH), (passE, passH)) <- (,) <$> newEvent <*> newEvent
                     listenTrans eE $ \e -> if f e then passH e else failH e 
                     pure (failE, passE)

instance Binary NominalDiffTime where
        get = fromRational <$> get
        put t = put ((toRational t) :: Rational)


checkRequest me t req@(Request n l r epk t' pK pH s c)
--    | l > roadLengthMax                 = Left "Rejected road : too long"
    | n > l                             = Left "Incorrect RequestPosition"
    | l /= length r                      = Left "Incorrect RoadLength"
 --   |  t - t' > maxDelay               = Left "Obsolete Request" 
    | r !! n /= me                       = Left "Not destinated to me"
--  | computeHashFromKey pK /= pH        = Left "Invalid tuple KeyHash/PubKey"
   -- | checkSig pK s $ requestHash req   = Right ()
    | otherwise                         = Left "Invalid signature"

