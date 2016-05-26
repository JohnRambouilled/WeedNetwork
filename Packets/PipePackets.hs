{-# LANGUAGE DeriveGeneric #-}
module Packets.PipePackets where
import Types.Crypto 

import Reactive.Banana.Frameworks
import GHC.Generics
import Data.Binary
import Data.Time.Clock
import Data.Time.Clock.POSIX

type Time = POSIXTime

roadLengthMax = 10 :: Number
maxDelay = 20 :: Time

type Number = Int
type SourceID = KeyHash
type UserID = KeyHash
type Road = [SourceID]
type PipeID = KeyHash

-- | Meta-type created to join incoming and outgoing request (both are treated equally by the Pipe and Routing layer)
--   the only difference being in the extraction of the pipe Key. Outgoing request comes with a signing function (sender)
data NewRequest = IncomingRequest {nrReq :: Request} |
                  OutgoingRequest {nrReq :: Request,
                                   nrSender :: PipeMessage -> PipePacket}

data Request = Request {reqPosition :: Number, -- ^ Position on the road, changed during routing (NOT SIGNED)
                        reqLength :: Number, -- ^ Total length of the road
                        reqRoad :: Road,  -- ^ Road : list of UserID
                        reqEPK :: DHPubKey,  -- ^ encrypted (Keyhash, PrivKey) for the destination of the road
                        reqTime :: Time,    -- ^ Send time of the request
                        reqPipeKey :: PubKey, -- ^ Public Key of the opening pipe
                        reqPipeID  :: PipeID,  -- ^ PipeID of the pipe (KeyHash of the Public Key)
                        reqPipeSig :: Signature,  -- ^ Signature of the packet's Hash
                        reqContent :: RawData}   -- ^ extra content if needed (cause why not)
    deriving Generic

type PipeMessage = (PipeID, Payload) --Left on a PipeClose

data PipePacket = PipePacket {pipeKeyID :: PipeID,  -- ^PipeID of the pipe used
                              pipeSig :: Signature,  -- ^ Signature of the packet
                              pipePosition :: Number, -- ^ Position on the pipe, changed during routing (NOT SIGNED)
                              pipeDirection :: Bool,  -- ^ Direction of the message (True being the direction followed by the request)
                              pipePayload :: Payload} |  -- ^ Content of the message (this one should be useful)
                  PipeClose  {pipeKeyID :: PipeID,  -- ^PipeID of the pipe used
                              pipeSig :: Signature, -- ^ Signature of the packet
                              pipePosition :: Number, -- ^ Position on the pipe, changed during routing (NOT SIGNED)
                              pipeDirection :: Bool, -- ^ Direction of the break (True being the direction followed by the request)
                              pipePayload :: Payload} -- ^ Content of the break
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
                                      scHash (PipeClose kH _ n b m) = encode (kH, n, b, m)
                                      scKeyHash = pipeKeyID
                                      scSignature = pipeSig
                                      scPushSignature p s = p{pipeSig = s}
instance Binary PipePacket

instance Binary NominalDiffTime where
        get = fromRational <$> get
        put t = put ((toRational t) :: Rational)



pipePacketTimeOut :: PipeID -> PipePacket
pipePacketTimeOut pID = PipeClose pID emptySignature 0 False $ encode "Pipe timed-out"

checkRequest :: UserID -> Time -> Request -> (Either String Request)
checkRequest = checkReq
    where checkReq me t req@(Request n l r epk t' pK pH s c)
            | l > roadLengthMax                 = Left "Rejected road : too long"
            | n > l-1                           = Left "Incorrect RequestPosition"
            | l /= length r                      = Left "Incorrect RoadLength"
            | t - t' > maxDelay                 = Left "Obsolete Request" 
            | r !! n /= me                       = Left "Not adressed to me"
            | computeHashFromKey pK /= pH        = Left "Invalid tuple KeyHash/PubKey"
            | checkSig pK req                   = Right req
            | otherwise                         = Left "Invalid signature"

