module Client.Sender where

import Types
import Packets
import Client.Crypto
import Client.WeedMonad

import Data.Binary

sendPipePacket :: PipeKeyPair -> PipeID -> KeyHash -> [PipeDataFlag] -> RawData -> WeedMonad ()
sendPipePacket pks id next flags datas = do c <- getClient
                                            signAndSend $ PipePacket (clUserID c) next header datas
    where header = PipeHeader id emptySignature flags

sendNeighData :: KeyHash -> L2 -> WeedMonad ()
sendNeighData kH l2 = do c <- getClient
                         signAndSend $ NeighData (clUserID c) kH emptySignature l2

sendNeighIntro :: WeedMonad ()
sendNeighIntro = do c <- getClient 
                    signAndSend $ NeighIntro (clUserID c) (fst $ clKeyPair c) emptySignature

signAndSend :: (SignedClass p, Binary p) => p -> WeedMonad ()
signAndSend p = do c <- getClient
                   clSender (c :: Client) . encode . sign (clKeyPair c) $ p
