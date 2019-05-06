module Client.Sender where

import Types
import Packets
import Client.Crypto
import Client.WeedMonad

import Data.Binary
import Control.Lens
import qualified Data.Map as M

type PipeSender = [PipePacketFlag] -> ComPacket -> WeedMonad ()

sendComMessage :: PipeSender -> ComID -> RawData -> WeedMonad ()
sendComMessage sender cID = sender [] . ComPmessage . ComData cID


genPipeSender :: PipeID -> WeedMonad (Maybe PipeSender)
genPipeSender pID = do pipeMap <- stmRead clLocalPipes
                       case M.lookup pID pipeMap of
                                Nothing -> logM "Client.Sender" "genPipeSender" Fail "Generating pipe sender for unkown pipeID" >> pure Nothing
                                Just e -> do sourceMap <- stmRead clDestinaries
                                             case _locPipeSource e `M.lookup` sourceMap of
                                                Nothing -> logM "Client.Sender" "genPipeSender" Error "Known pipe leading to unknown source" >> pure Nothing
                                                Just se -> let keys = _destPipeKeys se
                                                           in pure . Just $ \f -> sendPipePacket keys pID (_locPipeNeigh e) f . encode . PPCComPacket

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
                              keys <- clKeyPair <$> getClient
                              let cert = RessourceCert uk t val rID emptySignature
                                  ans = sign keys $ Answer cert ttl [uID] uID d
                              sendNeighData Broadcast $ L2Answer ans
                              

sendRawAnswer :: SourceID -> Road -> RessourceCert -> RessourceID -> Time -> TTL -> RawData -> WeedMonad ()
sendRawAnswer sID r cert rID val ttl d = do uk <- fst . clKeyPair <$> getClient
                                            uID <- clUserID <$> getClient
                                            let ans = Answer cert ttl (uID:r) sID d
                                            sendNeighData Broadcast $ L2Answer ans
                              

  
  
sendResearch :: RessourceID -> TTL -> Road -> RawData -> WeedMonad ()
sendResearch rID ttl r d = sendNeighData dest $ L2Research res
    where res = Research rID ttl r d
          dest = if null r then Broadcast else UserDest $ head r


-- | Send a request on the given road, using the provided pipePublicKey. Return the request to be pass to addLocalPipe.
-- | The road is provided in receveir convention (Client's userID first)
sendRequest :: Road -> PipeKeyPair -> RawData -> WeedMonad Request
sendRequest r pk d = do t <- getTime
                        uk <- fst . clKeyPair <$> getClient
                        let pipeID = computePipeID r'
                            req = signPipe pk $ Request pos r' uk t (fst pk) pipeID emptySignature d
                        logM "Client.Sender" "sendRequest" Normal $ "sending request : " ++ show req
                        sendNeighData (UserDest $ r !! pos) $ L2Request req
                        pure req
     where pos = length r - 2
           r' = reverse r


sendPipePacket :: PipeKeyPair -> PipeID -> KeyHash -> [PipePacketFlag] -> RawData -> WeedMonad ()
sendPipePacket pks id next flags datas = do c <- getClient
                                            let p = signPipe pks $ PipePacket (clUserID c) next id flags emptySignature datas
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
