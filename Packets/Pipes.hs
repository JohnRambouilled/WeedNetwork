{-# LANGUAGE DeriveGeneric #-}
module Packets.Pipes where
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




data Request = Request {reqPosition :: Number, -- ^ Position on the road, changed during routing (NOT SIGNED)
                        reqLength :: Number, -- ^ Total length of the road
                        reqRoad :: Road,  -- ^ Road : list of UserID
                        reqDHPubKey :: DHPubKey,  -- ^ DHPubKey of the origin of the request
                        reqTime :: Time,    -- ^ Send time of the request
                        reqPipeKey :: PubKey, -- ^ Public Key of the opening pipe
                        reqPipeID  :: PipeID,  -- ^ PipeID of the pipe (KeyHash of the Public Key)
                        reqPipeSig :: Signature,  -- ^ Signature of the packet's Hash
                        reqContent :: RawData}   -- ^ extra content if needed (cause why not)
    deriving Generic


data PipePacket = PipePacket {pipeKeyID :: PipeID,  -- ^PipeID of the pipe used
                              pipeSig :: Signature,  -- ^ Signature of the packet
                              pipePosition :: Number, -- ^ Position on the pipe, changed during routing (NOT SIGNED)
                              pipeDirection :: Bool,  -- ^ Direction of the message (True being the direction followed by the request)
                              pipePayload :: Payload} |  -- ^ Content of the message (this one should be useful)
                  PipeClose  {pipeKeyID :: PipeID,  -- ^PipeID of the pipe used
                              pipeSig :: Signature, -- ^ Signature of the packet
                              pipePosition :: Number, -- ^ Position on the pipe, changed during routing (NOT SIGNED)
                              pipeDirection :: Bool, -- ^ Direction of the break (True being the direction followed by the request)
                              pipePayload :: Payload}| -- ^ Content of the break
                  PipeControl{pipeKeyID :: PipeID,  -- ^PipeID of the pipe used
                              pipeSig :: Signature, -- ^ Signature of the packet
                              pipePosition :: Number, -- ^ Position on the pipe, changed during routing (NOT SIGNED)
                              pipeDirection :: Bool, -- ^ Direction of the break (True being the direction followed by the request)
                              pipeControl :: PipeControlMessage } -- ^ Content of the control message

    deriving Generic

data PipeControlMessage = PipeControlRefresh
    deriving Generic




instance Show Request where show (Request p l r _ t _ pID _ _) = "Request for pipe : " ++ show pID ++ " on road : " ++ show r ++" ("++ show p ++", "++ show l ++")"

instance SignedClass Request where scHash (Request n l r epk t pK pH s c) = encode (l,r,epk,t,pK,pH,c)
                                   scKeyHash = reqPipeID
                                   scSignature = reqPipeSig
                                   scPushSignature r s = r{reqPipeSig = s}
instance IntroClass Request where icPubKey = reqPipeKey
instance Binary Request
instance Binary PipeControlMessage


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

