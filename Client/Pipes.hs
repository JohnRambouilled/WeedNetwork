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


--onRequest :: UserID -> Request -> WeedMonad ()



checkRequest :: UserID -> Time -> Request -> Either String (Either (UserID, UserID) UserID)
checkRequest uID t req@(Request i r sk t0 pk (PipeID pid) s c)
    | i < 0 = Left "Negative Position"
    | length r <= i+2 = Left "Road is too short"
    | not $ checkPipeSig pk req = Left "Invalid signature"
    | t - t0 > maxRequestValidity = Left "Request expired"
    | pid /= computeHash r = Left "PipeID does not match road hash"
    | i == 0 = Right . Right . head $ tail r
    | otherwise = if uID == r' !! 1 then Right $ Left (head r', r' !! 2)
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
                                                            when isClose $ deleteRelayedPipe e
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
          deleteRelayedPipe e = stmModify clRelayedPipes (M.delete pID) >> killTimer (_relPipeTimer e)


-- | This function manages the content of a checked pipePacket. 
onPipeContent :: DestinaryEntry -> PipePacket -> WeedMonad ()
onPipeContent destE packet = case decodeOrFail $ _pipePacketData packet of
                                Left (_,_,s) -> logM "Client.Pipes" "onPipeContent" InvalidPacket ("Unable to decode content of a PipePacket : " ++ s)
                                Right (_,_, PPCPipePacket packet') -> onPipePacket packet'
                                Right (_,_, PPCL2 layer2) -> pure () -- TODO
                                Right (_,_, PPCComPacket comP) -> onComPacket (_destComModule destE) pID comP
    where pID = view pipePID packet
