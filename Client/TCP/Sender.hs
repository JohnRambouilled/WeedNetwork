{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
module Client.TCP.Sender where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.Suspend
import           Control.Concurrent.Timer
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import qualified Data.Array                  as A
import qualified Data.ByteString.Lazy        as B
import           Data.List

import           Data.Maybe
import           Data.Word
import           GHC.Int
import           Packets.TCP
import           Types.Crypto
{-| Idée : choisir les routes avec des colonies de fourmis |-}
data TCPCongestionController = TCPCongestionController {_congestionWindow :: TVar Int64} -- Differentiel max sur les seqnum dû au réseau
data TCPSender = TCPSender { _controller    :: TCPCongestionController,
                            _dataWindow     :: Int64, -- Differentiel max sur les seqnum dû au destinataire
                             _fragmentation :: Int, -- Taille maximale d'un paquet

                            _seqNum         :: Int64, -- SeqNum du prochain paquet

                            _dataToSend     :: RawData, -- donnée totale à envoyer
                            _position       :: Int64, --Position du premier octet restant à envoyer
                            _bufferSize     :: Int, -- Nombre de trames restante à ACK
                            _buffer         :: TVar [TCPPacket], -- buffer contenant les dernières trames en attente de ACK

                            _sendTimer      :: TimerIO}
makeLenses ''TCPSender
makeLenses ''TCPCongestionController

type TCPSenderT m x = (Monad m, MonadIO m) => StateT TCPSender m x

sendAllSegments :: [TCPPacket] -> IO ()
sendAllSegments segments = undefined


retransmit :: TCPSenderT m ()
retransmit = do pkts <- join $ liftIO . readTVarIO <$> use buffer
                liftIO $ sendAllSegments pkts
{-| Envoie le(s) prochain(s) segment(s) de la donnée à envoyer,
    Retourne True s'il ne reste plus de données à envoyer (aucun paquet envoyé)|-}
sendNextSegments :: TCPSenderT m Bool
sendNextSegments = do
  (toSend,ptr) <- (,) <$> use dataToSend <*> use position
  if (ptr == B.length toSend) then pure True
    else do
         (cwinM,dwin,frag) <- (,,) <$> use (controller . congestionWindow)<*> use dataWindow <*> use fragmentation
         cwin <- liftIO $ readTVarIO cwinM
         seqnum <- use seqNum
         let wmin = min cwin dwin -- Calcul de la taille de la prochaine session
             ptr' = min (B.length toSend - 1) (ptr + wmin - 1) -- détermination de l'offset sur le paquet
             rawSeg = B.index toSend <$> [ptr..ptr'] -- extraction des octets de la donnée à envoyer
             segmentPayloads = B.pack <$> groupBySize frag rawSeg -- fragmentation du payload
             (segments,nxtnum) = makeSegment (ptr' == B.length toSend -1) seqnum segmentPayloads -- génération des segments

         seqNum .= nxtnum -- on positionne le seqnum du prochain paquet (la fonction makeSegment le fixe à 0 et flag RST si besoin)
         position .= ptr'+1 -- on incrémente la position sur le buffer

         killTimer
         liftIO $ sendAllSegments segments -- on envoie les segments
         buffer <~ liftIO (newTVarIO segments)
         launchTimer
         pure False


  where

        killTimer ::TCPSenderT m ()
        killTimer = join $ liftIO.stopTimer <$> use sendTimer
        launchTimer :: TCPSenderT m Bool
        launchTimer = do sendTimer <~ liftIO newTimer
                         (buf,timer) <- (,) <$> use buffer <*> use sendTimer
                         liftIO $ repeatedStart timer  (readTVarIO buf >>= sendAllSegments)  $ sDelay 10

{-| Met à jour le buffer d'envoi à la reception d'un ACK.
    Met à jour la fenetre de data (maintenue par le client)
   Retourne True si la totalité du buffer a été transmise -}
onAck :: TCPPacket -> TCPSenderT m Bool
onAck pkt = do (buf,curwinsiz) <- (,) <$> use buffer <*> use dataWindow
               (ret,nxtsiz) <- liftIO $ atomically $ readTVar buf >>= process curwinsiz buf
               dataWindow .= nxtsiz

               if not ret then pure False
                 else sendNextSegments
  where -- Retourne Vrai si le buffer d'envoi est vide (plus de paquets en attente de ACK)
        -- ainsi que la nouvelle windowsiz si le paquet est accepté
        process curwinsiz bufT buf

         -- ne correspond pas à un ack
          | not $ _tcpAck (_tcpHeader pkt) = pure (False,curwinsiz)

          -- On vérifie si on attendait le paquet (Note : on utilise elem car même si les entêtes et le payload,
          -- sont différents, on compare uniquement avec le numéro de séquence
          | pkt `elem` buf = do updateWinSizeNewAck  -- on augmente la window size
                                let buf' = delete pkt buf -- on supprime le paquet de la liste de ceux qu'on attend
                                writeTVar bufT buf'
                                if null buf' then pure (True,winsize) else pure (False,winsize)
          -- Le paquet a déjà été confirmé. La windowsize de congestion est dégradée et le paquet rejeté.
          | otherwise = do updateWinSizeDuplicate >> pure (False,curwinsiz)
        winsize = _tcpWindowSize $ _tcpHeader pkt

        updateWinSizeNewAck = undefined -- TODO Augmente la winsize de congestion quand un paquet a bien été ACK
        updateWinSizeDuplicate = undefined -- TODO decroit la winsize de congestion quand un paquet à été reçu plusieurs fois

groupBySize n l = groupBySize' n l []
groupBySize' n [] r = r
groupBySize' n l r = let (prefix,ret) = Data.List.splitAt n l
                    in groupBySize' n ret $ r ++ [prefix]

{-| Construit un segment de données et positionne le flag PSH sur le dernier si spécifié.
    Positionne le flag RST du dernier paquet si la valeur max du seqnum est franchie
    Retourne le seqnum du prochain paquet|-}
makeSegment psh seqnum pkts = makeSegment' psh seqnum pkts []
makeSegment' :: Bool -> Int64 -> [RawData] -> [TCPPacket] -> ([TCPPacket],Int64)
makeSegment' psh seqnum [] ret = (ret,seqnum)
makeSegment' psh seqnum (x:[]) ret = (ret ++ [TCPPacket hdr x],if nxtnum >= maxSeqNum then 0 else nxtnum)
  where hdr = mkTCPHeader{_tcpSeqNum = seqnum, _tcpPsh = psh, _tcpRst = seqnum >= maxSeqNum}
        nxtnum = seqnum + B.length x
makeSegment' psh seqnum (x:xs) ret = makeSegment' psh (seqnum + B.length x) xs $ ret ++ [TCPPacket hdr x]
  where hdr = mkTCPHeader {_tcpSeqNum = seqnum}
