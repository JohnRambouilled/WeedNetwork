{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ExistentialQuantification #-}
module Crypto where
import Control.Monad.State
import qualified Data.Map as M
import Data.List 
import Data.Maybe
import Data.ByteString.Lazy hiding (null)
import Control.Concurrent
import Crypto.Random
import Codec.Crypto.RSA
import Codec.Crypto.RSA.Pure

import Class
import Packet
import Data.Binary
import System.Random
import Log

import Timer

type Hash = RawData


{- | CryptoModule : entry point of the netWork
        Manages two types of packet :
                * Introduce : contain a corresponding Key and KeyHash, and a Payload wich is used to determine the communication protocol.
                * DataPacket : containing only a KeyHash, they must be preceed by an Introduce (the content of the introduce is used to generate a callback for DataPackets)

    Every Packet is signed (and checked).
    Every KeyEntry is registered with a TimeOut, and is refreshed by Introduce. 
-}
type Crypto = MapModule KeyEntry KeyHash Packet Packet

type CryptoT = StateT Crypto

type CryptoCB a b = Behaviour Crypto a b


{- | Callback stored for a given key.
-}
type CryptoAction = KeyEntry -> CryptoCB Packet Packet



data KeyEntry = KeyEntry {pubKey :: PubKey,
                          keyCB :: [CryptoAction],
                          keyBreak :: IO ()}


data  DataCB = forall p . DataCB {hashFun :: HashFunction p, 
                             callBack :: CryptoCB p Packet}


type HashFunction p = Packet -> CryptoT IO (Maybe (Hash, p))

instance MapModules KeyEntry KeyHash Packet Packet where
        packetKey = pure . Just . keyID 
--        packetKey pkt = liftIO (Prelude.putStrLn "crypto : packetKey") >> pure (Just $ keyID pkt)
        entryBehaviour = keyCB





{- Main function for generating Callbacks on IntroducePacket from unknown keys. Arguments are :
        * MVar Crypto and Timer
        * TimeOut of the entry to be register fo the key
        * MVar containing the module to be called
        * Hash function to be applied to the packet (returning the entry type of the callback module)
        * Function to be applied to the result, returning : * Maybe a list of callbacks to register for the introduced key
                                                            * A list of Packets to relay
                                                            * An action to be called when the entry is deleted

  The time-Out is refreshed when an introducePacket is received from the registered key, then the callbacks are called.
-}
genCryptoCallback :: (Modules a p r) => MVar Crypto -> MVar Timer -> DiffTime -> MVar a -> HashFunction p -> (Packet -> r -> CryptoT IO (Maybe [CryptoAction], [Packet],IO ())) -> CryptoCB Packet Packet
genCryptoCallback crypto timer ttl aV hF oF  = genCallback aV inFun outFun 
        where inFun p = if checkIntroduce p then do ans <- maybeToList <$> checkHashFunction (key p) hF p
                                                    keepLog CryptoLog Normal $ if null ans then "Fail hashFunction" else "calling cryptoCallback"
                                                    pure ans
                                          else (keepLog CryptoLog Normal "not a correct introduce") >> pure [] 
              outFun p r = do (mDCBs, pL,onTimeOut) <- oF p r
                              case mDCBs of
                                Nothing -> (keepLog CryptoLog Normal "No callbacks to register") >> return pL
                                Just dCBL -> do (refresh, free) <- liftIO $ registerTimerM timer ttl ((runStateMVar crypto $ unregisterKeyEntry ( keyID p)) >> return False)
                                                let dCBL' = (\_ pkt  -> when (isIntroduce pkt) (liftIO refresh) >> pure []):dCBL
                                                keepLog CryptoLog Important $  "crypto : register callback for key :" ++ show (keyID p)
                                                registerDataCallback (keyID p) (key p) dCBL' (onTimeOut >> free) >> return pL
              checkIntroduce p = if isIntroduce p then pubKeyToHash (key p) == (runKeyHash $ keyID p) else False


-- | genCryptoCallback where the callback-module's MVar is transmitted to the output-Function.
genMCryptoCallback :: Modules a p r => MVar Crypto -> MVar Timer -> DiffTime -> MVar a -> HashFunction p -> (MVar a -> Packet -> r -> CryptoT IO (Maybe [CryptoAction], [Packet],IO ())) -> CryptoCB Packet Packet
genMCryptoCallback crypto timer ttl aV hF oFV = genCryptoCallback crypto timer ttl aV hF (oFV aV)


unregisterKeyEntry :: MonadIO m => KeyHash -> CryptoT m ()
unregisterKeyEntry kH = do kM' <- liftIO . removeAndClose =<< gets keyMap
                           modify $ \c ->  c{keyMap = kM'}
    where removeAndClose kM = let (kEM, kM') = M.updateLookupWithKey (pure $ pure Nothing) kH kM
                              in do maybe (pure ()) keyBreak kEM
                                    return kM'



-- | Insert callbacks for a given keyHash and pubkey. If an entry is already present, it keeps the previous key, and merge the callbacks.
registerDataCallback :: (Monad m) => KeyHash -> PubKey -> [CryptoAction] -> IO () -> CryptoT m ()
registerDataCallback kH k clbk free = insertMapBehaviourWith mergeEntry kH $ KeyEntry k clbk free
        where mergeEntry (KeyEntry k1 clbk1 free1) (KeyEntry _ clbk2 free2) = KeyEntry k1 (clbk1 ++ clbk2) (free1 >> free2)




-- | Transform a DataCB in cryptoAction : check the signature of the packet with checkHashFunction and call the callback with the returned value.
runDataCB :: DataCB -> CryptoAction
runDataCB (DataCB hF clbk) (KeyEntry k _ _) p = do 
        qM <- checkHashFunction k hF p
        case qM of
              Nothing -> pure []
              Just q -> clbk q
        

-- | Apply the hashFunction on the Packet. If a tupple (hash, value) is returned, check the signature for the given key and the hash, and return value if correct.
checkHashFunction :: PubKey -> HashFunction p -> Packet -> CryptoT IO (Maybe p)
checkHashFunction k hF p = do
        tM <-  hF p
        case tM of
            Nothing -> (keepLog CryptoLog Normal "hashFunction returned nothing") >> pure Nothing
            Just (h, q) -> if checkSignature k (sig p) h 
                             then return $ Just q
                             else (keepLog CryptoLog Suspect "Bad signature") >> pure Nothing



-- | Return the last 4 Bytes of the hash SHA256 of a given bytestring.
pubKeyToHash :: (Binary a) => a -> Hash
pubKeyToHash = computeHash . encode
   where computeHash = Data.ByteString.Lazy.drop 28 . hashFunction Codec.Crypto.RSA.hashSHA256

-- | Look for the Key in the map, and check the signature. Return False if the keyHash is unkown, or if the signature is not valid.
cryptoCheckSig :: (MonadIO m) => KeyHash -> Sig -> Hash -> CryptoT m Bool
cryptoCheckSig kH s h = do keepLog CryptoLog Normal $ "[cryptoCheckSig] :: checking signature for keyID : " ++ show kH
                           maybe (pure False) (\k -> pure $ checkSignature (pubKey k) s h) =<< (mapGetEntry kH) --return True

-- | Ckeck the validity of a signature.
checkSignature :: PubKey -> Sig -> Hash -> Bool
checkSignature pK s h = Codec.Crypto.RSA.verify (runPubKey pK) h s


sign :: PrivKey -> RawData -> RawData
sign uK h = Codec.Crypto.RSA.sign (runPrivKey uK) h

decrypt :: PrivKey -> RawData -> RawData
decrypt pK d = Codec.Crypto.RSA.decrypt (runPrivKey pK) d 

encrypt :: PubKey -> RawData -> IO RawData
encrypt pK d = do gen <- newGenIO :: IO SystemRandom
                  pure $ fst $ Codec.Crypto.RSA.encrypt gen (runPubKey pK) d

generateKeyPair :: Int -> IO (PubKey,PrivKey)
generateKeyPair keySiz = do (pubK,privK,_) <-  flip Codec.Crypto.RSA.generateKeyPair keySiz <$> (newGenIO :: IO SystemRandom)
                            return $ (PubKey pubK, PrivKey privK)


