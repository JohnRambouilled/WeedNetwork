module Client.Sender where

import Types
import Packets
import Client.Crypto
import Client.WeedMonad

import Data.Binary
import Control.Lens
import qualified Data.Map as M

researchMaxTTL = 10 :: TTL

type PipeSender = [PipePacketFlag] -> ComPacket -> WeedMonad ()

genPipeSender :: PipeID -> WeedMonad (Maybe PipeSender)
genPipeSender pID = do pipeMap <- stmRead clLocalPipes
                       case M.lookup pID pipeMap of
                                Nothing -> logM "Client.Sender" "genPipeSender" Fail "Generating pipe sender for unkown pipeID" >> pure Nothing
                                Just e -> do sourceMap <- stmRead clDestinaries
                                             case _locPipeSource e `M.lookup` sourceMap of
                                                Nothing -> logM "Client.Sender" "genPipeSender" Error "Known pipe leading to unknown source" >> pure Nothing
                                                Just se -> let keys = _destPipeKeys se
                                                           in pure . Just $ \f -> sendPipePacket keys pID (_locPipeNeigh e) f . encode

relayAnswer :: Answer -> WeedMonad ()
relayAnswer ans = if view ansTTL ans > 0 then relAns else error "Relayed Answer with a negative ttl"
    where relAns = do uID <- clUserID <$> getClient
                      let ans' = over ansTTL (+(-1)) $ over ansRoad (uID:) ans :: Answer
                      sendNeighData Broadcast $ L2Answer ans'


relayResearch :: Research -> WeedMonad ()
relayResearch res = if view resTTL res > 0 then sendNeighData dest $ L2Research res' else error "Relayed research with a negative ttl"
    where (dest, r) = case view resRoad res of
                          [] -> (Broadcast, [])
                          [_] -> (Broadcast, [])
                          _:xs -> (UserDest $ head xs, xs)
          res' = over resTTL (+(-1)) $ set resRoad r res
          

relayRequest :: Request -> WeedMonad ()
relayRequest req = sendNeighData dest $ L2Request req'
    where dest = UserDest $ view reqRoad req !! view reqPosition req'
          req' = over reqPosition (+(-1)) req


relayPipePacket :: PipePacket -> RelayedPipeEntry -> WeedMonad ()
relayPipePacket p e = do uID <- clUserID <$> getClient
                         let source = view pipeSource p
                             (prev, next) = (,) <$> view relPipePrevious <*> view relPipeNext $ e
                             dest = if source == prev then next
                                    else if source == next then prev
                                    else error "Relay pipe packet from a neighbourg absent from the pipe entry"
                         sendRawPacket . L1Pipe . set pipeSource uID $ set pipeDestinary dest p


sendAnswer :: RessourceID -> Time -> TTL -> RawData -> WeedMonad ()
sendAnswer rID val ttl d = do uk <- fst . clKeyPair <$> getClient
                              uID <- clUserID <$> getClient
                              t <- getTime
                              let cert = RessourceCert uk t val rID emptySignature
                                  ans = Answer cert ttl [uID] uID d
                              sendNeighData Broadcast $ L2Answer ans
                              
sendSimpleResearch :: RessourceID -> WeedMonad ()
sendSimpleResearch rID = sendResearch rID researchMaxTTL [] emptyPayload
  
sendResearch :: RessourceID -> TTL -> Road -> RawData -> WeedMonad ()
sendResearch rID ttl r d = sendNeighData dest $ L2Research res
    where res = Research rID ttl r d
          dest = if null r then Broadcast else UserDest $ head r


-- | Send a request on the given road, using the provided pipePublicKey. Return the pipeID as it is computed to forge the request.
sendRequest :: Road -> PipePubKey -> RawData -> WeedMonad PipeID
sendRequest r pk d = do t <- getTime
                        uk <- fst . clKeyPair <$> getClient
                        let pipeID = computePipeID r
                            req = Request (length r - 1) r uk t pk pipeID emptySignature d
                        sendNeighData (UserDest . head $ tail r) $ L2Request req
                        pure pipeID


sendPipePacket :: PipeKeyPair -> PipeID -> KeyHash -> [PipePacketFlag] -> RawData -> WeedMonad ()
sendPipePacket pks id next flags datas = do c <- getClient
                                            p <- signPacket $ PipePacket (clUserID c) next id flags emptySignature datas
                                            sendRawPacket $ L1Pipe p

sendNeighData :: NeighDestinary -> L2 -> WeedMonad ()
sendNeighData kH l2 = do c <- getClient
                         p <- signPacket $ NeighData (clUserID c) kH l2 emptySignature 
                         sendRawPacket $ L1Data p

sendNeighIntro :: WeedMonad ()
sendNeighIntro = do c <- getClient 
                    p <- signPacket $ NeighIntro (clUserID c) (fst $ clKeyPair c) emptySignature
                    sendRawPacket $ L1Intro p

signPacket :: SignedClass p => p -> WeedMonad p
signPacket p = do c <- getClient
                  pure $ sign (clKeyPair c) p

sendRawPacket :: L1 -> WeedMonad ()
sendRawPacket p = do c <- getClient
                     clSender c $ encode p

