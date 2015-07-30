{-# LANGUAGE DeriveGeneric #-}
module Routing where

import Data.ByteString.Lazy hiding (split,length)
import Data.Binary
import GHC.Generics
import Control.Monad
import qualified Data.Map as M
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Banana.Switch
import Control.Event.Handler

import Crypto
import Ressource

roadLengthMax = 10


buildRouting :: KeyHash -> Handler CryptoOrders -> Handler Request -> Handler Request -> Handler DataPacket -> RawData -> IO ()
buildRouting me ordH reqH sendReqH sendDataH raw = case decodeOrFail raw of
                                                                    Right (_,_,req) -> do time <- getTime 
                                                                                          case checkRequest me time req of
                                                                                            Right () -> if reqPosition req == reqLength req then reqH req
                                                                                                                else relayRequest ordH sendReqH sendDataH req
                                                                                            Left err -> print err
                                                                    Left (_,_,e) -> print e    
                                                               

relayRequest :: Handler CryptoOrders -> Handler Request -> Handler DataPacket ->  Handler Request
relayRequest ordH sendReqH sendDataH req@(Request pos _ _ _ _ pipeK pipeH  _ _) = do  ordH $ CryptoAdd pipeH $ CryptoEntry pipeK relayMessages
                                                                      --sendH $ Left (cnkPayload cnk){introContent = encodeRequest $ signAndRelay privK req}
                                                                                      sendReqH req{reqPosition = pos+1}
                                                                                                                                   
    where relayPipeMessage :: DataPacket -> Int -> PipeMessage -> IO ()
          relayPipeMessage dp pos' p@(PipeExit b d) = when (pos == pos') $ do ordH $ CryptoDelete pipeK
                                                                              sendDataH dp{dataContent=Payload (encode p) $ encode $ if b then pos + 1 else pos - 1}
          relayPipeMessage dp pos' p@(PipeData b d) = when (pos == pos') $ sendDataH dp{dataContent= Payload (encode p) $ encode $ if b then pos+1 else pos-1}
          relayMessages rawmsg = case decodePipeMessage $ dataContent rawmsg of
                                    Right (msg,pos') -> relayPipeMessage rawmsg pos' msg
                                    Left err -> print err



checkRequest me t req@(Request n l r epk t' pK pH s c)
    | l > roadLengthMax                 = Left "Rejected road : too long"
    | n > l                             = Left "Incorrect RequestPosition"
    | l /= length r                      = Left "Incorrect RoadLength"
    | (t - t') > maxDelay               = Left "Obsolete Request" 
    | r !! n /= me                       = Left "Not destinated to me"
    | computeHashFromKey pK /= pH        = Left "Invalid tuple KeyHash/PubKey"
    | checkSig pK s $ requestHash req   = Right ()
    | otherwise                         = Left "Invalid signature"

requestHash (Request n l r epk t pK pH s c) = encode (l,r,epk,t,pK,pH,c)

decodePipeMessage :: Payload -> Either String (PipeMessage,Number)
decodePipeMessage (Payload s us) = case (decodeOrFail s, decodeOrFail us) of
                                    (Right (_,_,msg), Right (_,_,pos)) -> Right (msg,pos)
                                    (Left (_,_,e), _) -> Left e
                                    (_, Left (_,_,e)) -> Left e
                                
                                    


type Number = Int
data Request = Request {reqPosition :: Number,
                        reqLength :: Number, -- ^ Total length of the road
                        reqRoad :: Road,  -- ^ Road : list of UserID
                        reqEPK :: RawData,  -- ^ encrypted (Keyhash, PrivKey) for the destination of the road
                        reqTime :: Time,
                        reqPipeKey :: PubKey,
                        reqPipeID  :: KeyHash,
                        reqPipeSig :: Signature,
                        reqContent :: RawData}
    deriving Generic

data SignedReq = SignedReq {srLength :: Number, -- ^ Total length of the road
                            srRoad :: Road,  -- ^ Road : list of UserID
                            srEPK :: RawData,  -- ^ encrypted (Keyhash, PrivKey) for the destination of the road
                            srTime :: Time,
                            srContent :: RawData}
    deriving Generic
    
data UnsignedReq = UnsignedReq {urPosition :: Number,
                                urNeighSig :: Signature}
    deriving Generic



data PipeMessage = PipeData {messageDirection :: Bool,
                             messageContent :: RawData} |
                   PipeExit {messageDirection :: Bool,
                             messageContent :: RawData} 
    deriving Generic


instance Binary Request
instance Binary SignedReq
instance Binary UnsignedReq
instance Binary PipeMessage
