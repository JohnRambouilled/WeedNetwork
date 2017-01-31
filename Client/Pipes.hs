module Client.Pipes where

import Types
import Packets
import Client.Crypto
import Client.Timer
import Client.WeedMonad
import Client.Sender
import Client.Communication

import Data.Binary
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Lens
import qualified Data.Map as M

-- | This function handle a raw unchecked PipePacket.
-- | We first check that the current Client is destinary of the packet.
-- |    If so, we look in the relayedPipesMap for the corresponding pipeID. 
-- |            If found, we check the signature of the packet, refresh the timer if the flag is present and relay the packet.
-- |            Else, we look in the localPipeMap.
-- |                if found, we check the signature, refresh the timer if necessary and call onPipeContent
-- |                Else, we ignore the packet (just like uncorrectly signed packets).
onPipePacket :: PipePacket -> WeedMonad ()
onPipePacket packet = do uID <- clUserID <$> getClient
                         if uID /= _pipeDestinary packet then pure ()
                         else do relMap <- stmRead clRelayedPipes
                                 case pID `M.lookup` relMap of
                                      Just e -> if checkPipeSig (_relPipePubKey e) packet 
                                                then do when isRefresh $ refreshTimer (_relPipeTimer e)
                                                        when isClose $ deleteRelayedPipe e
                                                        relayPipePacket  packet e
                                                else logM "Client.Pipes" "onPipeMessage" InvalidPacket "Signature invalid on relayed pipe packet"
                                      Nothing -> do eM <- getLocalEntries
                                                    case eM of 
                                                        Nothing -> logM "Client.Pipes" "onPipeMessage" InvalidPacket "Packet received from a unknown pipe."
                                                        Just (destE, locE) -> if checkPipeSig (fst $ _destPipeKeys destE) packet
                                                                                then do when isRefresh $ refreshTimer (_locPipeTimer locE)
                                                                                        onPipeContent destE packet
                                                                                else logM "Client.Pipes" "onPipeMessage" InvalidPacket "Signature invalid on received pipe packet. This should be worrying..."

    where pID = _pipeDID $ _pipePacketHeader packet
          getLocalEntries :: WeedMonad (Maybe (DestinaryEntry, LocalPipeEntry))
          getLocalEntries = do (locMap, destMap) <- (,) <$> stmRead clLocalPipes <*> stmRead clDestinaries
                               pure ( pID `M.lookup` locMap >>= \e -> ( (,) <$> _locPipeSource e `M.lookup` destMap <*> pure e) )
          isRefresh = PipeControlRefresh `elem` _pipeDFlags (_pipePacketHeader packet)
          isClose = PipeControlClose `elem` _pipeDFlags (_pipePacketHeader packet)
          deleteRelayedPipe e = stmModify clRelayedPipes (M.delete pID) >> killTimer (_relPipeTimer e)


-- | This function manages the content of a checked pipePacket. 
onPipeContent :: DestinaryEntry -> PipePacket -> WeedMonad ()
onPipeContent destE packet = case decodeOrFail $ _pipePacketData packet of
                                Left (_,_,s) -> logM "Client.Pipes" "onPipeContent" InvalidPacket ("Unable to decode content of a PipePacket : " ++ s)
                                Right (_,_, PPCPipePacket packet') -> onPipePacket packet'
                                Right (_,_, PPCL2 layer2) -> pure () -- TODO
                                Right (_,_, PPCComPacket comP) -> onComPacket (_destComModule destE) pID comP
    where pID = _pipeDID $ _pipePacketHeader packet
