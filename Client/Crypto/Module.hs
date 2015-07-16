{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ExistentialQuantification #-}
module Client.Crypto.Module where
import Control.Monad.State
import qualified Data.Map as M

import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import Log

import Client.Class
import Client.Packet


{- | CryptoModule : entry point of the netWork
        Manages two types of packet :
                * Introduce : contain a corresponding Key and KeyHash, and a Payload wich is used to determine the communication protocol.
                * DataPacket : containing only a KeyHash, they must be preceed by an Introduce (the content of the introduce is used to generate a callback for DataPackets)

    Every Packet is signed (and checked).
    Every KeyEntry is registered with a TimeOut, and is refreshed by Introduce. 
-}
type Crypto = MapModule KeyEntry KeyHash Packet Packet

--type CryptoT = StateT Crypto

type CryptoCB a b = Behaviour Crypto a IO b


{- | Callback stored for a given key.
-}
type CryptoAction = KeyEntry -> CryptoCB Packet [Packet]



data KeyEntry = KeyEntry {pubKey :: PubKey,
                          keyCB :: [CryptoAction],
                          keyBreak :: IOLog ()}


data  DataCB = forall p . DataCB {hashFun :: HashFunction p, 
                             callBack :: CryptoCB p [Packet]}


type Hash = RawData
type HashFunction p = CryptoCB Packet (Maybe (Hash, p))

instance MapModules KeyEntry KeyHash Packet Packet where
        packetKey = Just . keyID <$> ask
        entryBehaviour b = map ($b) $ keyCB b


unregisterKeyEntry :: (LogIO m, MonadState Crypto m) => KeyHash -> m ()
unregisterKeyEntry kH = do kM' <- removeAndClose =<< gets keyMap
                           modify $ \c ->  c{keyMap = kM'}
    where removeAndClose kM = let (kEM, kM') = M.updateLookupWithKey (pure $ pure Nothing) kH kM
                              in do tell =<< maybe (pure []) ((snd <$>) . liftIO . runWriterT . keyBreak) kEM
                                    return kM'



-- | Insert callbacks for a given keyHash and pubkey. If an entry is already present, it keeps the previous key, and merge the callbacks.
registerDataCallback :: MonadState Crypto m => KeyHash -> PubKey -> [CryptoAction] -> IOLog () -> m ()
registerDataCallback kH k clbk free = insertMapBehaviourWith mergeEntry kH $ KeyEntry k clbk free
        where mergeEntry (KeyEntry k1 clbk1 free1) (KeyEntry _ clbk2 free2) = KeyEntry k1 (clbk1 ++ clbk2) (free1 >> free2)




