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

type RoutingMap = EventEntryMap KeyHash PipePacket
type RoutingMapBhv t = ModEvent t RoutingMap


data Routing t = Routing {routingLocMap :: RoutingMapBhv t,
                          routingRelMap :: RoutingMapBhv t,
                          routingNewPipes :: Event t NewPipe,
                          routingRelayedPackets :: Event t PipePacket,
                          routingLocClose :: Handler PipeID,
                          routingRelClose :: Handler PipeID,
                          routingLogs :: Event t String}




buildRouting :: Frameworks t => UserID -> KeyPair -> Event t Request -> Event t PipePacket -> Moment t (Routing t)
buildRouting uID uK reqEuc packetE = do
                     t <- liftIO $ getPOSIXTime
                     (reqLogs, reqE) <- splitEither $ checkRequest uID t <$> reqEuc
                     (locClE, locClH) <- newEvent
                     (relClE, relClH) <- newEvent
                     --On Separe les requetes relayé et locales 
                     (reqRelE, reqLocE) <- splitEvent isLocalRequest reqE 
                     --Event des messages relayés
                     (relPE, relPH) <- newEvent
                     --On construit les cryptoMaps correspondantes
                     (relayMap, _) <- buildCryptoMap reqRelE packetE
                     (localMap, newPipeE) <- buildCryptoMap reqLocE packetE
                     --On bind les actions de relai (et de fermeture des pipes relayés)
                     relEvents <- mergeEvents $ meChanges relayMap
                     reactimate $ relayPackets relPH relClH <$> relEvents 
                     -- On ferme les pipes local sur PipeClose
                     -- listen des closes 
                     reactimate $ meModifier localMap . M.delete <$> locClE
                     reactimate $ meModifier relayMap . M.delete <$> relClE
                     --Retour de la structure de Routing
                     pure $ Routing localMap relayMap  (filterJust (makeNewPipe <$> newPipeE)) relPE locClH relClH ( ("Rejected request : " ++) <$> reqLogs)
    where relayPackets :: Handler PipePacket -> Handler KeyHash -> PipePacket -> IO ()
          relayPackets h _ p@(PipePacket _ _ n b _ ) = h p{pipePosition = if b then n + 1 else n -1} 
          relayPackets _ h (PipeClose kID _ _ _ _) = h kID
          isLocalRequest req = reqPosition req == reqLength req
          makeNewPipe (req, (EventEntry _ e _)) = requestToNewPipe uK req $ makePipeMessage <$> e

data NewPipe = NewPipe {npRoad :: Road,
                        npSource :: SourceID,
                        npPubKey :: PubKey,
                        npSender :: PipeMessage -> PipePacket,
                        npPipeID :: PipeID,
                        npTime :: Time,
                        npContent :: RawData,
                        npMessageEvent :: AddHandler PipeMessage}




routOpenPipe :: Routing t -> PipeID -> (DHPubKey, KeyPair) -> Road -> RawData -> IO (Request, NewPipe)
routOpenPipe rout pID (dhPk, (pK, sK)) r cnt = do 
        eE <- newEventEntry $ checkSig pK
        meModifier (routingLocMap rout) $ M.insert pID eE
        t <- getPOSIXTime
        let np  = NewPipe r (last r) pK (pipeMessageToPipePacket 0 True (pK,sK)) pID t cnt (makePipeMessage <$> eAddHandler eE)
            req = Request 0 (length r) r dhPk t pK pID emptySignature cnt
        pure (sign (pK, sK) req, np)


requestToNewPipe :: KeyPair -> Request -> AddHandler PipeMessage -> Maybe NewPipe
requestToNewPipe (upK,uk) (Request n _ r epk t pK pID _ cnt) e = (\s -> NewPipe r (head r) pK s pID t cnt e) <$> sender
            where sender = pipeMessageToPipePacket n False <$> decryptPrivKey epk uk

pipeMessageToPipePacket :: Number -> Bool -> KeyPair -> PipeMessage -> PipePacket
pipeMessageToPipePacket n b pK (Left  (pID,d)) = sign pK $ PipeClose  pID emptySignature n b d 
pipeMessageToPipePacket n b pK (Right (pID,d)) = sign pK $ PipePacket pID emptySignature n b d 



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
    | n > l                             = Left "Incorrect RequestPosition"
    | l /= length r                      = Left "Incorrect RoadLength"
    | t - t' > maxDelay                 = Left "Obsolete Request" 
    | r !! n /= me                       = Left "Not adressed to me"
    | computeHashFromKey pK /= pH        = Left "Invalid tuple KeyHash/PubKey"
    | checkSig pK req                   = Right req
    | otherwise                         = Left "Invalid signature"

