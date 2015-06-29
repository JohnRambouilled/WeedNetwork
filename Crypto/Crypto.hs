{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ExistentialQuantification #-}
module Crypto.Crypto where
import Control.Monad.State
import Data.List 
import Data.ByteString.Lazy hiding (null)
import Crypto.Random
--import Codec.Crypto.RSA
import qualified Crypto.PubKey.RSA.PKCS15 as C
import Crypto.PubKey.HashDescr hiding (HashFunction)
import Crypto.PubKey.RSA

import Class
import Packet
import Data.Binary
import Log
import Crypto.Module



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
   where computeHash = Data.ByteString.Lazy.drop 28 . fromStrict . hashFunction hashDescrSHA1 . toStrict

-- | Look for the Key in the map, and check the signature. Return False if the keyHash is unkown, or if the signature is not valid.
cryptoCheckSig :: (MonadIO m) => KeyHash -> Sig -> Hash -> CryptoT m Bool
cryptoCheckSig kH s h = do keepLog CryptoLog Normal $ "[cryptoCheckSig] :: checking signature for keyID : " ++ show kH
                           maybe (pure False) (\k -> pure $ checkSignature (pubKey k) s h) =<< (mapGetEntry kH) --return True

-- | Ckeck the validity of a signature.
checkSignature :: PubKey -> Sig -> Hash -> Bool
--checkSignature pK s h = Codec.Crypto.RSA.verify (runPubKey pK) h s
--checkSignature pK s h = rsassa_pkcs1_v1_5_verify hashSHA1 (runPubKey pK) h s
checkSignature pK s h = C.verify hashDescrSHA1 (runPubKey pK) (toStrict h) (toStrict s)


sign :: PrivKey -> RawData -> RawData
--sign uK h = Codec.Crypto.RSA.sign (runPrivKey uK) h
sign uK = either (\_->empty) fromStrict . C.sign Nothing hashDescrSHA1 (runPrivKey uK) . toStrict

decrypt :: PrivKey -> RawData -> RawData
decrypt pK =  either (\_->empty) fromStrict . C.decrypt Nothing (runPrivKey pK) . toStrict

encrypt :: PubKey -> RawData -> IO RawData
encrypt pK d = do gen <- cprgCreate <$> createEntropyPool :: IO SystemRNG
                  pure . either (\_->empty) fromStrict . fst . C.encrypt gen (runPubKey pK) $ toStrict d


generateKeyPair :: Int -> IO (PubKey,PrivKey)
generateKeyPair keySiz = do gen <- cprgCreate <$> createEntropyPool :: IO SystemRNG
                            let (pubK,privK) = fst $ generate gen keySiz 65537
                            return $ (PubKey pubK, PrivKey privK)


