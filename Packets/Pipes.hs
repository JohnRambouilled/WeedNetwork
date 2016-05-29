{-# LANGUAGE DeriveGeneric #-}
module Packets.Pipes where
import Types.Crypto 

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


data PipeData = PipeData {pipeDKeyID :: PipeID,  -- ^PipeID of the pipe used
                          pipeDSig :: Signature,  -- ^ Signature of the packet
                          pipeDPosition :: Number, -- ^ Position on the pipe, changed during routing (NOT SIGNED)
                          pipeDDirection :: Bool,  -- ^ Direction of the message (True being the direction followed by the request)
                          pipeDPayload :: Payload}  -- ^ Content of the message (this one should be useful)
              deriving Generic
data PipeControl = PipeControl{pipeCKeyID :: PipeID,  -- ^PipeID of the pipe used
                               pipeCSig :: Signature, -- ^ Signature of the packet
                               pipeCPosition :: Number, -- ^ Position on the pipe, changed during routing (NOT SIGNED)
                               pipeCDirection :: Bool, -- ^ Direction of the break (True being the direction followed by the request)
                               pipeCControl :: PipeControlMessage } -- ^ Content of the control message

              deriving Generic

data PipeControlMessage = PipeControlRefresh
                        | PipeClose RawData
              deriving Generic


data PipePacket = PipePacketData PipeData | PipePacketControl PipeControl
            deriving Generic
instance Show Request where show (Request p l r _ t _ pID _ _) = "Request for pipe : " ++ show pID ++ " on road : " ++ show r ++" ("++ show p ++", "++ show l ++")"

instance SignedClass Request where scHash (Request n l r epk t pK pH s c) = encode (l,r,epk,t,pK,pH,c)
                                   scKeyHash = reqPipeID
                                   scSignature = reqPipeSig
                                   scPushSignature r s = r{reqPipeSig = s}
instance IntroClass Request where icPubKey = reqPipeKey
instance Binary Request
instance Binary PipeControlMessage
instance Binary PipeData
instance Binary PipeControl


instance SignedClass PipeData where scHash (PipeData kH _ n b m) = encode (kH, n, b, m)
                                    scKeyHash = pipeDKeyID
                                    scSignature = pipeDSig
                                    scPushSignature p s = p{pipeDSig = s}

instance SignedClass PipeControl where scHash (PipeControl kH _ n b m) = encode (kH, n, b, m)
                                       scKeyHash = pipeCKeyID
                                       scSignature = pipeCSig
                                       scPushSignature p s = p{pipeCSig = s}

instance SignedClass PipePacket where  scHash (PipePacketData pl) = scHash pl
                                       scHash (PipePacketControl pl) = scHash pl
                                       scKeyHash (PipePacketData pl) = scKeyHash pl
                                       scKeyHash (PipePacketControl pl) = scKeyHash pl
                                       scSignature (PipePacketData pl) = scSignature pl
                                       scSignature (PipePacketControl pl) = scSignature pl
                                       scPushSignature (PipePacketData pl)  = PipePacketData . scPushSignature pl
                                       scPushSignature (PipePacketControl pl)  = PipePacketControl . scPushSignature pl
                                       
instance Binary PipePacket

instance Binary NominalDiffTime where
        get = fromRational <$> get
        put t = put ((toRational t) :: Rational)



--pipePacketTimeOut :: PipeID -> PipePacket
--pipePacketTimeOut pID = PipeClose pID emptySignature 0 False $ encode "Pipe timed-out"
