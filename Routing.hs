{-# LANGUAGE DeriveGeneric #-}
module Routing where

import Crypto 
import Class
import Timer

import Reactive.Banana
import Reactive.Banana.Frameworks
import GHC.Generics
import Data.Binary
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.Clock.POSIX

roadLengthMax = 10 :: Number
maxDelay = 20 :: Time


type Number = Int
type SourceID = KeyHash
type UserID = KeyHash
type Road = [SourceID]
type PipeID = KeyHash

type RoutingMap = EventEntryMap UserID PipePacket
type RoutingMapBhv t = ModEvent t RoutingMap


data Routing t = Routing {routingLocMap :: RoutingMapBhv t,
                          routingRelMap :: RoutingMapBhv t,

                          routingNewPipes :: Event t NewPipe,
                          routingRelayedPackets :: Event t PipePacket,
                          routingOutgoingRequest :: Event t Request,

                          routingLocClose :: Handler PipeID,
                          routingRelClose :: Handler PipeID,

                          routingLogs :: Event t String}

pipeTimeOut = 10 :: Time


buildRouting :: Frameworks t => UserID -> DHPrivKey -> Event t NewRoad -> Event t Request -> Event t PipePacket -> Moment t (Routing t)
buildRouting uID dhSK newRE reqEuc packetE = do
                     t <- liftIO $ getPOSIXTime
                     (reqLogs, reqE) <- splitEither $ checkRequest uID t <$> reqEuc
                     (locClE, locClH) <- newEvent
                     (relClE, relClH) <- newEvent
                     --On Separe les requetes relayé et locales 
                     (reqRelE, reqLocE) <- splitEvent isLocalRequest reqE 
                     --Event des messages relayés
                     (relPE, relPH) <- newEvent
                     --Ouverture des pipes locaux
                     reqOutE <- routOpenPipe newRE
                     --On construit les cryptoMaps correspondantes
                     (relayMap, cryptoRelE) <- buildCryptoMap pipeTimeOut newRoutingEntry reqRelE relClE packetE
                     let reqRelOE = relayRequest . fst . fromRight <$> (filterE isRight cryptoRelE)
                         newReqE = union (IncomingRequest <$> reqLocE) reqOutE
                         packetOut = union reqRelOE $ nrReq <$> reqOutE
                     (localMap, cryptoLocE) <- buildCryptoMap pipeTimeOut newRoutingEntry newReqE locClE packetE
                     --On bind les actions de relai (et de fermeture des pipes relayés)
                     relEvents <- mergeEvents $ meChanges relayMap
                     reactimate $ relayPackets relPH relClH <$> relEvents 
                     -- On ferme les pipes local sur PipeClose
                     --Creation des event de Pipe, push des TimeOut
                     (actE, newPipeE) <- splitEither $ makeNewPipe <$> cryptoLocE
                     reactimate actE
                     --Retour de la structure de Routing
                     let logs = unions [("AcceptedRequest : " ++) . show <$> reqE,
                                        ("Rejected request : " ++) <$> reqLogs]
                     pure $ Routing localMap relayMap newPipeE relPE packetOut locClH relClH logs
    where relayPackets :: Handler PipePacket -> Handler KeyHash -> PipePacket -> IO ()
          relayPackets h _ p@(PipePacket _ _ n b _ ) = h p{pipePosition = if b then n + 1 else n -1} 
          relayPackets _ h (PipeClose kID _ _ _ _) = h kID
          relayRequest :: Request -> Request
          relayRequest r = r{reqPosition = reqPosition r + 1}
          newRoutingEntry _ = newEventEntry $ pure True
          isLocalRequest req = reqPosition req == reqLength req - 1
          makeNewPipe (Right (req, EventEntry _ e _) ) = maybe (Left $ pure ()) Right $ newRequestToNewPipe dhSK req $ makePipeMessage <$> e
          makeNewPipe (Left (pID, EventEntry h _ _) ) = Left . h $ pipePacketTimeOut pID
          



routOpenPipe :: Frameworks t => Event t NewRoad -> Moment t (Event t NewRequest)
routOpenPipe newRE = do 
                        (reqE, reqH) <- newEvent
                        reactimate $ onNewRoad reqH <$> newRE
                        pure $ reqE
    where onNewRoad :: Handler NewRequest -> NewRoad -> IO ()
          onNewRoad send nr = do (dhPK, dhSK) <- generateDHKeyPair
                                 case decryptKeyPair (nrDHPubKey nr) dhSK of
                                        Nothing -> pure ()
                                        Just (pK,sK) -> do t <- getPOSIXTime
                                                           let (r,cnt) = ( (,) <$> nrRoad <*> nrContent ) $ nr
                                                               pID = computeHashFromKey pK
                                                               req = sign (pK,sK) $ Request 1 (length r) r dhPK t pK pID emptySignature cnt
                                                               sender = pipeMessageToPipePacket 1 True (pK,sK)
                                                           send $ OutgoingRequest req sender

newRequestToNewPipe :: DHPrivKey -> NewRequest -> AddHandler PipeMessage -> Maybe NewPipe
newRequestToNewPipe uk (IncomingRequest (Request n _ r epk t pK pID _ cnt)) e = (\s -> NewPipe r (head r) pK s pID t cnt e) <$> sender
            where sender = pipeMessageToPipePacket n False <$> decryptKeyPair epk uk
newRequestToNewPipe _ (OutgoingRequest (Request _ _ r _ t pK pID _ cnt) sender) e = Just $ NewPipe r (head r) pK sender pID t cnt e

pipeMessageToPipePacket :: Number -> Bool -> KeyPair -> PipeMessage -> PipePacket
pipeMessageToPipePacket n b pK (Left  (pID,d)) = sign pK $ PipeClose  pID emptySignature n b d 
pipeMessageToPipePacket n b pK (Right (pID,d)) = sign pK $ PipePacket pID emptySignature n b d 

data NewPipe = NewPipe {npRoad :: Road,
                        npSource :: SourceID,
                        npPubKey :: PubKey,
                        npSender :: PipeMessage -> PipePacket,
                        npPipeID :: PipeID,
                        npTime :: Time,
                        npContent :: RawData,
                        npMessageEvent :: AddHandler PipeMessage}

data NewRoad = NewRoad {nrRoad :: Road,
                        nrDHPubKey :: DHPubKey,
                        nrSourceID :: SourceID,
                        nrContent :: RawData}


--requestToNewRoad :: Request -> NewRoad
--requestToNewRoad = NewRoad <$> reqPipeID <*> reqRoad <*> reqEPK <*> last . reqRoad <*> reqContent

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


instance Show Request where show (Request p l r _ t _ pID _ _) = "Request for pipe : " ++ show pID ++ " on road : " ++ show r ++" ("++ show p ++", "++ show l ++")"

instance SignedClass Request where scHash (Request n l r epk t pK pH s c) = encode (l,r,epk,t,pK,pH,c)
                                   scKeyHash = reqPipeID
                                   scSignature = reqPipeSig
                                   scPushSignature r s = r{reqPipeSig = s}
instance IntroClass Request where icPubKey = reqPipeKey
instance Binary Request

data NewRequest = IncomingRequest {nrReq :: Request} |
                  OutgoingRequest {nrReq :: Request,
                                   nrSender :: PipeMessage -> PipePacket}
instance SignedClass NewRequest where scHash = scHash . nrReq
                                      scKeyHash = scKeyHash . nrReq
                                      scSignature = scSignature . nrReq
                                      scPushSignature nr s = nr{nrReq = scPushSignature (nrReq nr) s}
instance IntroClass NewRequest where icPubKey = reqPipeKey . nrReq


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

pipePacketTimeOut :: PipeID -> PipePacket
pipePacketTimeOut pID = PipeClose pID emptySignature 0 False $ encode "Pipe timed-out"

instance Show PipePacket where
    show (PipePacket kID _ n b _) = "PipePacket on pipe : " ++ show kID ++ " pos " ++ show n ++ (if b then "+" else "-")
    show (PipeClose  kID _ n b _) = "PipeClose  on pipe : " ++ show kID ++ " pos " ++ show n ++ (if b then "+" else "-")


instance SignedClass PipePacket where scHash (PipePacket kH _ n b m) = encode (kH, n, b, m)
                                      scKeyHash = pipeKeyID
                                      scSignature = pipeSig
                                      scPushSignature p s = p{pipeSig = s}
instance Binary PipePacket

type PipeMessage = Either (PipeID, Payload) (PipeID, Payload) --Left on a PipeClose
makePipeMessage :: PipePacket -> PipeMessage
makePipeMessage (PipePacket pID _ _ _ p) = Right (pID,p)
makePipeMessage (PipeClose  pID _ _ _ p) = Left  (pID,p)

instance Binary NominalDiffTime where
        get = fromRational <$> get
        put t = put ((toRational t) :: Rational)

checkRequest :: UserID -> Time -> Request -> Either String Request
checkRequest me t req@(Request n l r epk t' pK pH s c)
    | l > roadLengthMax                 = Left "Rejected road : too long"
    | n > l-1                           = Left "Incorrect RequestPosition"
    | l /= length r                      = Left "Incorrect RoadLength"
    | t - t' > maxDelay                 = Left "Obsolete Request" 
    | r !! n /= me                       = Left "Not adressed to me"
    | computeHashFromKey pK /= pH        = Left "Invalid tuple KeyHash/PubKey"
    | checkSig pK req                   = Right req
    | otherwise                         = Left "Invalid signature"

