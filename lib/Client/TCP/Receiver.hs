{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
module Client.TCP.Receiver where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import qualified Data.Array           as A
import qualified Data.ByteString.Lazy as B
import           Data.Int
import           Data.List

import           Data.Ix
import           Data.Maybe
import           Packets.TCP
import           Types.Crypto

{-| On accuse reception de chaque paquet qu'on bufferise avec un ACK + numero de séquence du paquet reçu
    On calcule le seqnum du prochain paquet attendu (nxtnum)
    Si le seqnum du prochain paquet reçu est nxtnum (le paquet est celui qu'on attend), on translate nxtnum = nxtnum + lengh(pkt)
          De plus, on regarde s'il permet de compléter un fragment à l'aide des segments dans le désordre.
    Si le seqnum du prochain paquet reçu est >= nxtnum (paquet out of order):
                         nxtnum < seqnum <= nxtnum + windowSize, alors le paquet est bufferisé (on envoie un ACK)
                         seqnum >= nxtnum + windowSize, alors le paquet est drop (on n'envoie pas de ACK)
            dans les deux cas, un duplicateAck est envoyé pour que la source degrade la congestion window

Idée : Il faudrait que les ACK empruntent la même route que les paquets associés pour faciliter le controle
      de la congestion

-}

data Buffer = Buffer { _buffer         :: A.Array Int (Maybe TCPPacket),
                       _firstFreeIndex :: Int,
                       _currentSize    :: Int64,
                       _maxSize        :: Int64}
data TCPReceiver = TCPReceiver { _consecBuf    :: Buffer, -- Dernier segments reçus dans l'ordre (trié)
                                 _notConsecBuf :: Buffer, -- Derniers segments reçus dans le desordre (non trié)
                                 _prevNum      :: Int64, -- Seqnum du dernier segment accepté
                                 _nextNum      :: Int64, -- Seqnum du prochain segment attendu
                                 _pushFunction :: RawData -> IO ()} -- Callback de la couche supérieure



type TCPReceiverT m x = (MonadIO m) => StateT TCPReceiver m x
makeLenses ''Buffer
makeLenses ''TCPReceiver


rawAddSegment :: TCPPacket -> Buffer -> Buffer
rawAddSegment pkt (Buffer buf free siz maxsiz) = Buffer buf' (free+1) (siz + B.length  (_tcpPayload pkt)) maxsiz
  where buf' = buf A.// [(free, Just pkt)]


freeSize :: Buffer -> Int64
freeSize (Buffer buf free siz maxsiz)
  |free > snd (A.bounds buf) = 0
  | otherwise  = min 0 (maxsiz - siz)

bufferElems :: Buffer -> [TCPPacket]
bufferElems (Buffer buf free _ _) = fromJust <$> [buf A.! i | i <- [b1 .. min b2 free]]
  where (b1,b2) = A.bounds buf

{-| Retourne les segments du buffer qui sont consecutifs au paquet spécifié,
    vide le buffer, et retourne le buffer modifié.
    Le premier paquet du fragment retourné correspond a celui passé en argument.
    Note : les segments non consécutifs avec un seqnum < à celui du paquet sont réputés être obsolètes et donc discard|-}
addConsecutiveSegment :: TCPPacket -> Buffer -> ([TCPPacket],Buffer)
addConsecutiveSegment pkt nconsec = (firstConsecFrag,nconsec')
  where consecP p1 p2 = _tcpSeqNum ( _tcpHeader p1) + B.length (_tcpPayload p1) == _tcpSeqNum (_tcpHeader p2)
        sortedNConsec = (pkt:) $ dropWhile ((\p -> _tcpSeqNum (_tcpHeader pkt)
                                                  <= _tcpSeqNum (_tcpHeader pkt) + B.length (_tcpPayload pkt))) $
                                             sort (bufferElems nconsec) -- On élimine les paquets obsolètes (dont le seqnum est anterieur à pkt+size(pkt))
        (firstConsecFrag,rest) = over _2 concat $
                                         (,) <$> head <*> tail $ groupBy consecP sortedNConsec
        buf = _buffer nconsec
        buf' = A.array (A.bounds buf) $ [(i,Nothing) | i <- range (A.bounds buf)]
        nconsec' = foldr rawAddSegment (nconsec{_firstFreeIndex = 1, _currentSize=0, _buffer = buf'}) rest


{-| Vide le buffer consecutif |-}
flushBuffer :: TCPReceiverT m ()
flushBuffer = do
  (buf,push) <- (,) <$> use consecBuf <*> use pushFunction
  let free = _firstFreeIndex buf
      bs = B.concat $ _tcpPayload <$> bufferElems buf
  consecBuf . firstFreeIndex .= 1
  consecBuf . currentSize .= 0
  liftIO $ push bs

currentWindowSize :: TCPReceiverT m Int64
currentWindowSize = do
  (nxtNum,consec) <- (,) <$> use nextNum <*> use consecBuf
  pure $ nxtNum + _maxSize consec - _currentSize consec
onNewSegment :: TCPPacket -> TCPReceiverT m ()
onNewSegment pkt = do
  (nxtNum,winsiz) <- (,) <$> use nextNum <*> currentWindowSize

  (consec,nconsec) <- (,) <$> use consecBuf <*> use notConsecBuf
  if nxtNum == _tcpSeqNum (_tcpHeader pkt)
    then onConsecutiveSegment pkt consec nconsec nxtNum
    else onNotConsecutiveSegment pkt consec nconsec nxtNum winsiz
  pure ()
  where onConsecutiveSegment pkt consec nconsec nxtNum
          | freeSize consec < B.length (_tcpPayload pkt) = flushBuffer >> onNewSegment pkt -- pas assez de place dans le buffer consecutif
          | otherwise = let (frag,rest) = addConsecutiveSegment pkt nconsec -- On determine les fragments consécutifs dans le buffer nconsec
                        in do   sendAck -- On accuse reception
                                notConsecBuf .= rest -- On retire de nconsec le fragment consecutif au paquet reçu
                                forM_ frag $ \ seg -> do -- On ajoute les segments du fragment à consec
                                  (cursiz,maxsiz) <- (,) <$> use (consecBuf . currentSize) <*> use (consecBuf . maxSize)
                                  when (cursiz + B.length (_tcpPayload seg) < maxsiz) $ flushBuffer -- Si consec n'a plus de place, on le flush avant d'ajouter le paquet

                                  consecBuf %= rawAddSegment seg
                                  prevNum .= _tcpSeqNum (_tcpHeader seg) -- On met a jour le dernier seqnum reçu
                                  nextNum += (B.length $ _tcpPayload seg) -- On met à jour le prochain seqnum attendu
                                  when (_tcpRst $ _tcpHeader seg) $ -- Si le flag RST est placé, on reset le numero de séquence
                                    nextNum .= 0
                                  when (_tcpPsh $ _tcpHeader seg) $ -- Si le flag PSH est placé, on flush le buffer
                                    flushBuffer



        {- Si le paquet n'est pas celui attendu, on l'ajoute au buf
           des segments non consecutifs et on envoie
           un duplicate ACK -}
        onNotConsecutiveSegment pkt consec nconsec nxtNum winsiz
          | freeSize nconsec < B.length (_tcpPayload pkt) = missingAck
          -- On ajoute le paquet s'il appartient à la fenêtre que l'on avait autorisée.
          | otherwise = do
              when (nxtNum <= _tcpSeqNum (_tcpHeader pkt)
                    && _tcpSeqNum (_tcpHeader pkt) <= nxtNum + winsiz) $ do notConsecBuf %= rawAddSegment pkt
                                                                            sendAck -- Accuse reception du paquet si on l'ajoute
              missingAck -- Prévient qu'on n'a pas reçu un paquet anterieur

        --TODO
        missingAck = undefined -- Envoie le ACK du dernier paquet consécutif reçu (avec la bonne winsize)
        sendAck = undefined -- Envoie le ACK de pkt (avec la bonne winsize)

{-
isBufferNotFull :: TCPPacket -> TCPReceiver -> Bool
isBufferNotFull pkt recv = _bufferCurrentSize recv + B.length (_tcpPayload pkt) <= _bufferMaxSize recv
                         && _bufferFirstFreeIndex recv <= snd (A.bounds $ _buffer recv)


{-| Push tout le buffer |-}
flushBuffer :: TCPReceiverT m ()
flushBuffer = do (buf,push,size) <- (,,) <$> use buffer <*> use pushFunction <*> use bufferFirstFreeIndex
                 let bs = B.concat $ _tcpPayload <$> sort [fromJust $ buf A.! i | i <- [1..size-1]]
                 liftIO $ push bs
                 bufferFirstFreeIndex .= 0
                 bufferCurrentSize .= 0

maximalSeqNum :: TCPReceiverT m Int64
maximalSeqNum = do
  (nxtNum, curSiz, maxSiz) <- (,,) <$> use nextNum <*> use bufferCurrentSize <*> use bufferMaxSize
  pure $ nxtNum + maxSiz - curSiz

{-| Ajoute le segment au buffer et met à jour les tailles et pointeurs |-}
addSegment :: TCPPacket -> TCPReceiverT m ()
addSegment pkt = do
  (bufI) <- use bufferFirstFreeIndex
  buffer %= (A.// [(bufI,Just pkt)]) -- Ajout du paquet au buffer
  bufferFirstFreeIndex += 1 -- Incrément du pointeur vers le prochain slot libre
  bufferCurrentSize += B.length (_tcpPayload pkt) -- Augmentation de la taille consommée

{-| Lorsque l'on ajoute un segment consécutif, on regarde s'il ne
    complète pas une chaîne consécutive plus grande.
    On parcourt les paquets à partir -}
onConsecutiveSegment :: TCPPacket -> TCPReceiverT m ()
onConsecutiveSegment pkt =  pure ()
onNewPacket :: TCPPacket -> TCPReceiverT m ()
onNewPacket pkt = do (seq,bufI) <- (,) <$> use nextNum  <*> use bufferFirstFreeIndex
                     isBufNotFull <- isBufferNotFull pkt <$> get
                     {- Si le buffer est plein, on le flush -}
                     when (not $ isBufNotFull) flushBuffer
                     {- Si le numéro de sequence est celui que l'on veut,
                        on met à jour le receiver -}
                     if (_tcpSeqNum (_tcpHeader pkt) == seq ) then do
                            buffer %= (A.// [(bufI,Just pkt)]) -- Ajout du paquet au buffer
                            bufferFirstFreeIndex += 1 -- Incrément du pointeur vers le prochain slot libre
                            bufferCurrentSize += B.length (_tcpPayload pkt) -- Augmentation de la taille consommée
                            prevNum .= _tcpSeqNum (_tcpHeader pkt) -- On met à jour la valeur du dernier seqnum reçu
                            nextNum .= nxtNum -- Determination du prochain seqnum attendu (0 si le paquet est flag RST)
                            when (_tcpPsh $ _tcpHeader pkt) flushBuffer -- Flush du buffer si le paquet est flag PSH
                            ack -- On accuse reception du paquet

                       {- Si le numéro de séquence du paquet ne coincide pas, on envoie un duplicate ack -}
                       else duplicateAck
   where nxtNum
           |_tcpRst $ _tcpHeader pkt = 0
           | otherwise = _tcpSeqNum (_tcpHeader pkt) + B.length (_tcpPayload pkt)
         {-| TODO - Accuse reception du paquet,
             ATTENTION: doit gérer la windowsize en fonction des buffers|-}
         ack = undefined
         duplicateAck = undefined


-}
