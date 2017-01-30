{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Client.TCP.Receiver where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import qualified Data.Array           as A
import qualified Data.ByteString.Lazy as B
import           Data.Int
import           Data.List            (groupBy)
import           Data.List.Ordered
import           Data.Maybe
import           Packets.TCP
import           Types.Crypto
type Buffer = A.Array Int (Maybe TCPPacket)

data TCPReceiver = TCPReceiver { _buffer               :: Buffer, -- Dernier segments reçus
                                 _nextNum              :: Int64, -- Seqnum du prochain paquet attendu
                                 _pushFunction         :: RawData -> IO (), -- Callback de la couche supérieure
                                 _bufferFirstFreeIndex :: Int, -- Index du prochain slot libre dans le buffer
                                 _bufferCurrentSize    :: Int64, -- Taille actuelle du buffer (octets)
                                 _bufferMaxSize        :: Int64} -- Taille max du buffer (octets)


type TCPReceiverT m x = (MonadIO m) => StateT TCPReceiver m x
makeLenses ''TCPReceiver

isBufferNotFull :: TCPPacket -> TCPReceiver -> Bool
isBufferNotFull pkt recv = _bufferCurrentSize recv + B.length (_tcpPayload pkt) <= _bufferMaxSize recv
                         && _bufferFirstFreeIndex recv <= snd (A.bounds $ _buffer recv)


{-| Push tout le buffer |-}
flushBuffer :: TCPReceiverT m ()
flushBuffer = do (buf,push,size) <- (,,) <$> use buffer <*> use pushFunction <*> use bufferFirstFreeIndex
                 let bs = B.concat $ _tcpPayload <$> [fromJust $ buf A.! i | i <- [1..size-1]]
                 liftIO $ push bs
                 bufferFirstFreeIndex .= 0
                 bufferCurrentSize .= 0

onNewPacket :: TCPPacket -> TCPReceiverT m ()
onNewPacket pkt = do (seq,bufI) <- (,) <$> use nextNum  <*> use bufferFirstFreeIndex
                     isBufNotFull <- isBufferNotFull pkt <$> get
                     {- Si le buffer est plein, on le flush -}
                     when (not $ isBufNotFull) flushBuffer
                     {- Si le numéro de sequence est celui que l'on veut,
                        on met à jour le receiver -}
                     when (_tcpSeqNum (_tcpHeader pkt) == seq ) $ do
                       buffer %= (A.// [(bufI,Just pkt)]) -- Ajout du paquet au buffer
                       bufferFirstFreeIndex += 1 -- Incrément du pointeur vers le prochain slot libre
                       bufferCurrentSize += B.length (_tcpPayload pkt) -- Augmentation de la taille consommée
                       nextNum .= nxtNum -- Determination du prochain seqnum attendu (0 si le paquet est flag RST)
                       when (_tcpPsh $ _tcpHeader pkt) flushBuffer -- Flush du buffer si le paquet est flag PSH
                       ack -- On accuse reception du paquet
   where nxtNum
           |_tcpRst $ _tcpHeader pkt = 0
           | otherwise = _tcpSeqNum (_tcpHeader pkt) + B.length (_tcpPayload pkt)
         {-| TODO - Accuse reception du paquet,
             ATTENTION: doit gérer la windowsize en fonction des buffers|-}
         ack = undefined
{-
{-| Push recursivement les fragments complets |-}
flushBuffer :: (RawData -> IO ()) -- fonction de la couche supérieure dans laquelle les fragments complets sont push
            -> [TCPPacket] -- liste des paquets
            -> IO [TCPPacket]
flushBuffer _ [] = pure []
flushBuffer push pkts = case nextPacketFromFrag frag of
  (Nothing,_) -> pure pkts
  (Just b, rest) -> do push b
                       flushBuffer push rest
  where frag = head $ groupBy consecutivesP pkts


-}

{-onNewPacket :: TCPPacket -> TCPReceiver -> IO TCPReceiver
onNewPacket pkt rcv = -}
