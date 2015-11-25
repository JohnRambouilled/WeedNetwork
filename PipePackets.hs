{-# LANGUAGE DeriveGeneric #-}
module PipePackets where
import Crypto 
import Timer

import Reactive.Banana.Frameworks
import GHC.Generics
import Data.Binary
import Data.Time.Clock

roadLengthMax = 10 :: Number
maxDelay = 20 :: Time

type Number = Int
type SourceID = KeyHash
type UserID = KeyHash
type Road = [SourceID]
type PipeID = KeyHash


data NewRequest = IncomingRequest {nrReq :: Request} |
                  OutgoingRequest {nrReq :: Request,
                                   nrSender :: PipeMessage -> PipePacket}

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

type PipeMessage = Either (PipeID, Payload) (PipeID, Payload) --Left on a PipeClose

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


instance Show Request where show (Request p l r _ t _ pID _ _) = "Request for pipe : " ++ show pID ++ " on road : " ++ show r ++" ("++ show p ++", "++ show l ++")"

instance SignedClass Request where scHash (Request n l r epk t pK pH s c) = encode (l,r,epk,t,pK,pH,c)
                                   scKeyHash = reqPipeID
                                   scSignature = reqPipeSig
                                   scPushSignature r s = r{reqPipeSig = s}
instance IntroClass Request where icPubKey = reqPipeKey
instance Binary Request

instance SignedClass NewRequest where scHash = scHash . nrReq
                                      scKeyHash = scKeyHash . nrReq
                                      scSignature = scSignature . nrReq
                                      scPushSignature nr s = nr{nrReq = scPushSignature (nrReq nr) s}
instance IntroClass NewRequest where icPubKey = reqPipeKey . nrReq


instance Show PipePacket where
    show (PipePacket kID _ n b _) = "PipePacket on pipe : " ++ show kID ++ " pos " ++ show n ++ (if b then "+" else "-")
    show (PipeClose  kID _ n b _) = "PipeClose  on pipe : " ++ show kID ++ " pos " ++ show n ++ (if b then "+" else "-")


instance SignedClass PipePacket where scHash (PipePacket kH _ n b m) = encode (kH, n, b, m)
                                      scKeyHash = pipeKeyID
                                      scSignature = pipeSig
                                      scPushSignature p s = p{pipeSig = s}
instance Binary PipePacket

instance Binary NominalDiffTime where
        get = fromRational <$> get
        put t = put ((toRational t) :: Rational)



makePipeMessage :: PipePacket -> PipeMessage
makePipeMessage (PipePacket pID _ _ _ p) = Right (pID,p)
makePipeMessage (PipeClose  pID _ _ _ p) = Left  (pID,p)

pipeMessageToPipePacket :: Number -> Bool -> KeyPair -> PipeMessage -> PipePacket
pipeMessageToPipePacket n b pK (Left  (pID,d)) = sign pK $ PipeClose  pID emptySignature n b d 
pipeMessageToPipePacket n b pK (Right (pID,d)) = sign pK $ PipePacket pID emptySignature n b d 

pipePacketTimeOut :: PipeID -> PipePacket
pipePacketTimeOut pID = PipeClose pID emptySignature 0 False $ encode "Pipe timed-out"

checkRequest :: UserID -> Request -> IO (Either String Request)
checkRequest i r = checkReq i <$> getTime <*> pure r
    where checkReq me t req@(Request n l r epk t' pK pH s c)
            | l > roadLengthMax                 = Left "Rejected road : too long"
            | n > l-1                           = Left "Incorrect RequestPosition"
            | l /= length r                      = Left "Incorrect RoadLength"
            | t - t' > maxDelay                 = Left "Obsolete Request" 
            | r !! n /= me                       = Left "Not adressed to me"
            | computeHashFromKey pK /= pH        = Left "Invalid tuple KeyHash/PubKey"
            | checkSig pK req                   = Right req
            | otherwise                         = Left "Invalid signature"

