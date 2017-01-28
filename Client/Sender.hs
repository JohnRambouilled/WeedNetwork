module Client.Sender where

import Types
import Packets
import Client.Crypto
import Client.WeedMonad

import Data.Binary


relayAnswer :: Answer -> WeedMonad ()
relayAnswer ans = if ansTTL ans > 0 then relAns else error "Relayed Answer with a negative ttl"
    where relAns = do uID <- clUserID <$> getClient
                      let r = uID : ansRoad ans 
                          ans' = ans{ansTTL = ansTTL ans - 1, ansRoad = r}
                      sendNeighData Broadcast $ L2Answer ans'


relayResearch :: Research -> WeedMonad ()
relayResearch res = if resTTL res > 0 then sendNeighData dest $ L2Research res' else error "Relayed research with a negative ttl"
    where (dest, r) = case resRoad res of
                          [] -> (Broadcast, [])
                          _:[] -> (Broadcast, [])
                          _:xs -> (UserDest $ head xs, xs)
          res' = res{resTTL = resTTL res - 1, resRoad = r}
          

relayRequest :: Request -> WeedMonad ()
relayRequest req = sendNeighData dest $ L2Request req'
    where pos = reqPosition req + 1
          dest = UserDest $ reqRoad req !! pos
          req' = req{reqPosition = pos}


sendAnswer :: RessourceID -> Time -> TTL -> RawData -> WeedMonad ()
sendAnswer rID val ttl d = do uk <- fst . clKeyPair <$> getClient
                              uID <- clUserID <$> getClient
                              t <- getTime
                              let cert = RessourceCert uk t val rID emptySignature
                                  ans = Answer cert ttl [uID] uID d
                              sendNeighData Broadcast $ L2Answer ans
                              

sendResearch :: RessourceID -> TTL -> Road -> RawData -> WeedMonad ()
sendResearch rID ttl r d = sendNeighData dest $ L2Research res
    where res = Research rID ttl r d
          dest = if null r then Broadcast else UserDest $ head r


-- | Send a request on the given road, using the provided pipePublicKey. Return the pipeID as it is computed to forge the request.
sendRequest :: Road -> PipePubKey -> RawData -> WeedMonad PipeID
sendRequest r pk d = do t <- getTime
                        uk <- fst . clKeyPair <$> getClient
                        let pipeID = computePipeID r
                            req = Request 1 (length r) r uk t pk pipeID emptySignature d
                        sendNeighData (UserDest . head $ tail r) $ L2Request req
                        pure pipeID


sendPipePacket :: PipeKeyPair -> PipeID -> KeyHash -> [PipeDataFlag] -> RawData -> WeedMonad ()
sendPipePacket pks id next flags datas = do c <- getClient
                                            signAndSend $ PipePacket (clUserID c) next header datas
    where header = PipeHeader id emptySignature flags

sendNeighData :: NeighDestinary -> L2 -> WeedMonad ()
sendNeighData kH l2 = do c <- getClient
                         signAndSend $ NeighData (clUserID c) kH emptySignature l2

sendNeighIntro :: WeedMonad ()
sendNeighIntro = do c <- getClient 
                    signAndSend $ NeighIntro (clUserID c) (fst $ clKeyPair c) emptySignature

signAndSend :: (SignedClass p, Binary p) => p -> WeedMonad ()
signAndSend p = do c <- getClient
                   clSender c . encode . sign (clKeyPair c) $ p
