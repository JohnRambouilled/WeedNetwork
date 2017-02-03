module Client.Pipes where

import Types
import Packets
import Client.Crypto
import Client.Timer
import Client.WeedMonad
import Client.Sender
import Client.Communication

import Data.Binary
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Lens
import qualified Data.Map as M

maxRequestValidity = 10 :: Time
relayedPipeTimeOut = 30 :: Time



onRequest :: UserID -> Request -> WeedMonad ()
onRequest sID req = do (t,uID) <- (,) <$> getTime <*> fmap clUserID getClient
                       case checkRequest uID t req of
                            Left s -> logM "Client.Pipes" "onRequest" InvalidPacket $ "Invalid request received : " ++ s
                            Right (Left (p,n)) -> if p == sID then onRelayedRequest p n req
                                                               else logM "Client.Pipes" "onRequest" InvalidPacket "Invalid Relayed request received : source of packet doesn't match the road"
                            Right (Right p) -> if p == sID then onLocalRequest p req
                                                          else logM "Client.Pipes" "onRequest" InvalidPacket "Invalid Local Request received : source of packet doesn't match the road"
                                                            

onLocalRequest :: UserID -> Request -> WeedMonad ()
onLocalRequest prev req = undefined

-- | Verify that the pipeID is not already in use.
-- | If not, create a new timer 
onRelayedRequest :: UserID -> UserID -> Request -> WeedMonad ()
onRelayedRequest prev next req = do relMap <- stmRead clRelayedPipes 
                                    case pID `M.lookup` relMap of
                                        Just _ -> logM "Client.Pipes" "onRelayedRequest" InvalidPacket "Request for already used pipeID"
                                        Nothing -> do timer <- createTimer relayedPipeTimeOut (removeRelayedPipe pID >> pure ()) 
                                                      stmModify clRelayedPipes $ M.insert pID (entry timer)
                                                      logM "Client.Pipes" "onRelayedRequest" Normal ("New relayed pipe : " ++ show pID ++ " have been added.")
                                                      relayRequest req
        where pID = _reqPipeID req
              entry = RelayedPipeEntry (_reqPipeKey req) prev next


-- | Check a request validity. Return Left with an error if the request is incorrect.
-- | otherwise, it return either a tuple (previous, next) of the two adjacent relay on the road, or a single userID in the case of a local request.
checkRequest :: UserID -> Time -> Request -> Either String (Either (UserID, UserID) UserID)
checkRequest uID t req@(Request i r sk t0 pk (PipeID pid) s c)
    | i < 0 = Left "Negative Position"
    | length r <= i+2 = Left "Road is too short"
    | not $ checkPipeSig pk req = Left "Invalid signature"
    | t - t0 > maxRequestValidity = Left "Request expired"
    | pid /= computeHash r = Left "PipeID does not match road hash"
    | i == 0 = Right . Right . head $ tail r
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
                         if uID /= _pipeDestinary packet then pure ()
                         else do (relMap, neighMap) <- (,) <$> stmRead clRelayedPipes <*> stmRead clNeighbours
                                 if isNothing $ M.lookup sID neighMap then logM "Client.Pipes" "onPipePacket" InvalidPacket "PipePacket from an unknown neighbour."
                                 else case pID `M.lookup` relMap of   
                                        Just e -> if checkPipeSig (_relPipePubKey e) packet 
                                                    then do when isRefresh $ refreshTimer (_relPipeTimer e)
                                                            when isClose $ removeRelayedPipe pID >> pure ()
                                                            relayPipePacket  packet e
                                                    else logM "Client.Pipes" "onPipePacket" InvalidPacket "Signature invalid on relayed pipe packet."
                                        Nothing -> do eM <- getLocalEntries
                                                      case eM of 
                                                            Nothing -> logM "Client.Pipes" "onPipePacket" InvalidPacket "Packet received from a unknown pipe."
                                                            Just (destE, locE) -> if checkPipeSig (fst $ _destPipeKeys destE) packet
                                                                                    then do when isRefresh $ refreshTimer (_locPipeTimer locE)
                                                                                            onPipeContent destE packet
                                                                                    else logM "Client.Pipes" "onPipePacket" InvalidPacket "Signature invalid on received pipe packet. This should be worrying..."

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
                                             killTimer (_relPipeTimer e)
                                             pure True


-- | This function manages the content of a checked pipePacket. 
-- | For now, only ComPacket are managed. other packet will make the program crash [TODO] ############################################
onPipeContent :: DestinaryEntry -> PipePacket -> WeedMonad ()
onPipeContent destE packet = case decodeOrFail $ _pipePacketData packet of
                                Left (_,_,s) -> logM "Client.Pipes" "onPipeContent" InvalidPacket ("Unable to decode content of a PipePacket : " ++ s)
                                Right (_,_, PPCPipePacket packet') -> undefined
                                Right (_,_, PPCL2 layer2) -> undefined
                                Right (_,_, PPCComPacket comP) -> onComPacket (_destComModule destE) pID comP
    where pID = view pipePID packet
