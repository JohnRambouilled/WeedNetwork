module Client.Pipes where

import Types
import Packets
import Client.Crypto
import Client.Timer
import Client.WeedMonad
import Client.Sender
import Client.Communication
import Client.Destinary
import Types.Graph.RoadGraph
import Types.Graph.Type

import Data.Binary
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Lens
import qualified Data.Map as M

maxRequestValidity = 5 :: Time
pipeTimeOut = 10 :: Time
pipeRefreshTime = 5 :: Time


-- | open a Pipe on a given Road. Require the Public Key of the destinary.
openPipe :: Road -> RawData -> PubKey -> WeedMonad (Maybe PipeID)
openPipe r cnt sK = do dEM <- destinaryInsertPipes sK sID [pID] 
                       case dEM of
                            Nothing -> return Nothing
                            Just dE -> let pipeK = view destPipeKeys dE in
                                         do req <- sendRequest r pipeK cnt 
                                            logM "Client.pipes" "openPipe" Normal $ "Opening pipe to source : " ++ show sID ++ " with pipeID : " ++ show pID
                                            timer <- newTimer pipeTimeOut (void $ removeLocalPipe pID)
                                            repeater <- newRepeater pipeRefreshTime $ refresh pipeK timer
                                            pipeE <- addLocalPipe (Just repeater) timer req 
                                            return (Just pID)
  where pID = computePipeID $ reverse r
        sID = last r
        nID = head $ tail r
        refresh k t = do sendPipePacket k pID nID [PipeControlRefresh] (encode PPCPing) >> refreshTimer t
                         logM "Client.Pipes" "openPipe" Normal $ "Refreshing pipe : " ++ show pID ++ " to neigh " ++ show nID 

onRequest :: UserID -> Request -> WeedMonad ()
onRequest sID req = do (t,uID) <- (,) <$> getTime <*> fmap clUserID getClient
                       case checkRequest uID t req of
                            Left s -> logM "Client.Pipes" "onRequest" InvalidPacket $ "Invalid request received : " ++ s
                            Right (Left (p,n)) -> if p == sID then onRelayedRequest p n req
                                                               else logM "Client.Pipes" "onRequest" InvalidPacket "Invalid Relayed request received : source of packet doesn't match the road"
                            Right (Right p) -> if p == sID then onLocalRequest req
                                                          else logM "Client.Pipes" "onRequest" InvalidPacket "Invalid Local Request received : source of packet doesn't match the road"
                                                            

onLocalRequest :: Request -> WeedMonad ()
onLocalRequest req = do locMap <- stmRead clLocalPipes
                        logM "Client.Pipes" "onLocalRequest" Normal $ "Reveiced local request from : " ++ show sID ++ " for pipe : " ++ show pID
                        case pID `M.lookup` locMap of
                                    Just _ -> logM "Client.Pipes" "onLocalRequest" InvalidPacket "Request for an already used pipeID"
                                    Nothing -> do dEM <- destinaryInsertPipes (_reqSourceKey req) sID [pID]
                                                  case dEM of
                                                    Just _ -> do timer <- newTimer pipeTimeOut (void $ removeLocalPipe pID)
                                                                 void $ addLocalPipe Nothing timer req
                                                    Nothing -> pure ()
        where pID = view reqPipeID req
              sID = last $ view reqRoad req



removeLocalPipe :: PipeID -> WeedMonad Bool
removeLocalPipe pID = do locMap <- stmRead clLocalPipes 
                         case pID `M.lookup` locMap of
                                Nothing -> do logM "Client.Pipes" "removeLocalPipe" Error $ "Pipe does not exist : " ++ show pID
                                              pure False
                                Just e -> do logM "Client.Pipes" "removeLocalPipe" Normal $ "Removing pipe : " ++ show pID
                                             destinaryRemovePipe (_locPipeSource e) pID
                                             stmModify clLocalPipes $ M.delete pID
                                             uID <- clUserID <$> getClient
                                             stmModify clGraph $ deletePipe pID (keyHash2VertexID uID)
                                             killTimer (_locPipeTimer e)
                                             pure True
                                             


-- | Add a local pipe entry to the pipeMap and to the graph.
-- | A pipe is local of the client is at an extremity (and therefore posess the key).
-- | Parameter are : * repeater (only for outgoing pipes, the origin of the request is responsible for maintaining the pipe)
-- |                 * timer entry to add to the map
-- |                 * request (not sent by this function)
-- | WARNING : raises error if called with a request with empty road
addLocalPipe :: Maybe TimerEntry -> TimerEntry -> Request -> WeedMonad LocalPipeEntry
addLocalPipe rpM tE req = do stmModify clLocalPipes $ M.insert pID entry
                             uVertex <- keyHash2VertexID . clUserID <$> getClient 
                             stmModify clGraph $ insertPipe uVertex Local pID (map keyHash2VertexID r)
                             logM "Client.Pipes" "addLocalPipe" Normal ("New local " ++ (if out then "outgoing" else "incoming") ++ " pipe : " ++ show pID ++ " have been added to source : " ++ show sID ++ " with neighbour : " ++ show prev)
                             pure entry
        where pID = _reqPipeID req
              r = _reqRoad req
              out = isJust rpM
              sID = if out then head r else last r 
              prev  = if out then last $ init r else head $ tail r
              entry = LocalPipeEntry prev tE rpM sID

                                                  

-- | Verify that the pipeID is not already in use.
-- | If not, create a new timer calling removeRelayedPipe, forge a relayedPipeEntry
onRelayedRequest :: UserID -> UserID -> Request -> WeedMonad ()
onRelayedRequest prev next req = do relMap <- stmRead clRelayedPipes 
                                    case pID `M.lookup` relMap of
                                        Just _ -> logM "Client.Pipes" "onRelayedRequest" InvalidPacket "Request for an already used pipeID"
                                        Nothing -> do timer <- newTimer pipeTimeOut (removeRelayedPipe pID >> pure ()) 
                                                      addRelayedPipe timer prev next req
                                                      logM "Client.Pipes" "onRelayedRequest" Normal ("New relayed pipe : " ++ show pID ++ " have been added.")
                                                      relayRequest req
        where pID = _reqPipeID req


addRelayedPipe :: TimerEntry -> UserID -> UserID -> Request -> WeedMonad ()
addRelayedPipe tE prev next req = do stmModify clRelayedPipes $ M.insert pID entry
                                     uVertex <- keyHash2VertexID . clUserID <$> getClient 
                                     stmModify clGraph $ insertPipe uVertex Relayed pID (map keyHash2VertexID $ reverse r)
        where pID = _reqPipeID req
              r = _reqRoad req
              entry = RelayedPipeEntry (view reqPipeKey req) prev next tE


keyHash2VertexID :: KeyHash -> VertexID
keyHash2VertexID (KeyHash u) = VertexID u


-- | Check a request validity. Return Left with an error if the request is incorrect.
-- | otherwise, it return either a tuple (previous, next) of the two adjacent relay on the road, or a single userID in the case of a local request.
checkRequest :: UserID -> Time -> Request -> Either String (Either (UserID, UserID) UserID)
checkRequest uID t req@(Request i r sk t0 pk (PipeID pid) s c)
    | i < 0 = Left "Negative Position"
    | length r < i+2 = Left $ "Road is too short : " ++ show i ++ " / " ++ show (length r)
    | not $ checkPipeSig pk req = Left "Invalid signature"
    | t - t0 > maxRequestValidity = Left "Request expired"
    | pid /= computeHash r = Left "PipeID does not match road hash"
    | i == 0 = if uID == head r then Right . Right . head $ tail r  -- Incoming pipe
                                else Left "UserID at Position in Road doesn't match"
    | otherwise = if uID == r' !! 1 then Right $ Left (r' !! 2, head r')
                                    else Left "UserID at Position in Road doesn't match"
            where r' = drop (i-1) r
                                               

-- | This function handle a raw unchecked PipePacket.
-- | We first check that the current Client is destinary of the packet, and that the source belongs to the NeighboursMap
-- |    If so, we look in the relayedPipesMap for the corresponding pipeID. 
-- |            If found, we check the signature of the packet, refresh the timer if the flag is present and relay the packet.
-- |            Else, we look in the localPipeMap.
-- |                if found, we check the signature, refresh the timer if necessary and call onPipeContent
-- |                Else, we ignore the packet (just like uncorrectly signed packets).
onPipePacket :: PipePacket -> WeedMonad ()
onPipePacket packet = do uID <- clUserID <$> getClient
                         if uID /= _pipeDestinary packet then logM "Client.Pipes" "onPipePacket" Normal "PipePacket not adressed to me"
                         else do (relMap, neighMap) <- (,) <$> stmRead clRelayedPipes <*> stmRead clNeighbours
                                 if isNothing $ M.lookup sID neighMap then logM "Client.Pipes" "onPipePacket" InvalidPacket "PipePacket from an unknown neighbour."
                                 else case pID `M.lookup` relMap of   
                                        Just e -> if checkPipeSig (_relPipePubKey e) packet 
                                                    then do when isRefresh $ refreshTimer (_relPipeTimer e)
                                                            when isClose $ removeRelayedPipe pID >> pure ()
                                                            logM "Client.Pipes" "onPipePacket" Normal $ "relaying PipePacket on Pipe " ++ show pID 
                                                            relayPipePacket  packet e
                                                    else logM "Client.Pipes" "onPipePacket" InvalidPacket "Signature invalid on relayed pipe packet."
                                        Nothing -> do eM <- getLocalEntries
                                                      case eM of 
                                                            Nothing -> logM "Client.Pipes" "onPipePacket" InvalidPacket "Packet received from a unknown pipe."
                                                            Just (destE, locE) -> if checkPipeSig (fst $ _destPipeKeys destE) packet
                                                                                    then do when isRefresh $ do refreshTimer $ view locPipeTimer locE
                                                                                                                logM "Client.Pipes" "onPipePacket" Normal $ "Refreshing pipe : " ++ show pID
                                                                                            onPipeContent destE packet
                                                                                    else logM "Client.Pipes" "onPipePacket" InvalidPacket $ "Signature invalid on received pipe packet. This should be worrying... " ++ show (fst $ _destPipeKeys destE)

    where (pID, sID)  = (,) <$> _pipePID <*> _pipeSource $ packet
          getLocalEntries :: WeedMonad (Maybe (DestinaryEntry, LocalPipeEntry))
          getLocalEntries = do (locMap, destMap) <- (,) <$> stmRead clLocalPipes <*> stmRead clDestinaries
                               pure ( pID `M.lookup` locMap >>= \e ->  (,) <$> _locPipeSource e `M.lookup` destMap <*> pure e) 
          isRefresh = PipeControlRefresh `elem` _pipeFlags packet
          isClose = PipeControlClose `elem` _pipeFlags packet


removeRelayedPipe :: PipeID -> WeedMonad Bool
removeRelayedPipe pID = do m <- stmRead clRelayedPipes
                           case pID `M.lookup` m of
                                Nothing -> pure False
                                Just e -> do stmModify clRelayedPipes (M.delete pID) 
                                             uID <- clUserID <$> getClient
                                             stmModify clGraph $ deletePipe pID (keyHash2VertexID uID)
                                             killTimer (_relPipeTimer e)
                                             pure True


-- | This function manages the content of a checked pipePacket. 
-- | For now, only ComPacket are managed. other packet will make the program crash [TODO] ############################################
onPipeContent :: DestinaryEntry -> PipePacket -> WeedMonad ()
onPipeContent destE packet = case decodeOrFail $ _pipePacketData packet of
                                Left (_,_,s) -> logM "Client.Pipes" "onPipeContent" InvalidPacket ("Unable to decode content of a PipePacket : " ++ s)
                                Right (_,_, PPCPipePacket packet') -> logM "Client.Pipes" "onPipeContent" Error "Pipe-on-Pipe not implemented yet..."
                                Right (_,_, PPCL2 layer2) -> logM "Client.Pipes" "onPipeContent" Error "Layer2-on-Pipe not implemented yet..."
                                Right (_,_, PPCComPacket comP) -> onComPacket (_destComModule destE) pID comP
                                Right (_,_, PPCPing) -> pure ()
    where pID = view pipePID packet
