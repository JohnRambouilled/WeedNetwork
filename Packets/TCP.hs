{-# LANGUAGE TemplateHaskell #-}
module Packets.TCP where


import           Control.Lens
import qualified Data.Array           as A
import           Data.ByteString.Lazy hiding (delete, head, zip)
import           Data.Word
import           GHC.Int
import           Types.Crypto




data TCPHeader = TCPHeader { _tcpSeqNum     :: Int64, -- Position du premier octet du segment sur le datagramme final (indicé à partir de 0)
                             _tcpSyn        :: Bool,
                             _tcpAck        :: Bool,
                             _tcpRst        :: Bool,
                             _tcpPsh        :: Bool,
                             _tcpWindowSize :: Int64} -- Ne pas envoyer plus que windowSize sans avoir reçu de ACK


-- On compare les headers uniquement par rapport au seqnum
instance Eq TCPHeader where t1 == t2 = _tcpSeqNum t1 == _tcpSeqNum t2
instance Ord TCPHeader where t1 <= t2 = _tcpSeqNum t1 <= _tcpSeqNum t2
mkTCPHeader = TCPHeader 0 False False False False defaultWindowSize
defaultWindowSize = 1535

data TCPPacket = TCPPacket {_tcpHeader :: TCPHeader, _tcpPayload :: RawData}
-- Deux paquets TCP sont identiques s'ils ont le même seqnum
instance Eq TCPPacket where t1 == t2 = _tcpHeader t1 == _tcpHeader t2
instance Ord TCPPacket where t1 <= t2 = _tcpHeader t1 <= _tcpHeader t2


makeLenses ''TCPHeader
makeLenses ''TCPPacket


maxSeqNum :: Int64
maxSeqNum = 2^50

