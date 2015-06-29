{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ExistentialQuantification #-}
module Crypto.Crypto where
import Control.Monad.State
import Data.Maybe
import qualified Data.ByteString as BStrct
import qualified Data.ByteString.Lazy as B
import Crypto.Random
--import Codec.Crypto.RSA
import qualified Crypto.PubKey.RSA.PKCS15 as C
import Crypto.PubKey.HashDescr hiding (HashFunction)
import Crypto.PubKey.RSA
import Debug.Trace

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
pubKeyToHash :: (Show a, Binary a) => a -> Hash
pubKeyToHash a = trace ("Pubkeytohash : " ++ show a)  $ computeHash . encode $ a
   where computeHash x = let h = hashFunction hashDescrSHA1 $ trace ("computing hash of : " ) $ B.toStrict x
                         in B.take keyHashByteSize . B.reverse $ B.fromStrict $ trace ("hashlenght = " ++ (show $ BStrct.length h)) h
-- | Look for the Key in the map, and check the signature. Return False if the keyHash is unkown, or if the signature is not valid.
cryptoCheckSig :: (MonadIO m) => KeyHash -> Sig -> Hash -> CryptoT m Bool
cryptoCheckSig kH s h = do keepLog CryptoLog Normal $ "[cryptoCheckSig] :: checking signature for keyID : " ++ show kH
                           maybe (pure False) (\k -> pure $ checkSignature (pubKey k) s h) =<< (mapGetEntry kH) --return True

-- | Ckeck the validity of a signature.
checkSignature :: PubKey -> Sig -> Hash -> Bool
--checkSignature pK s h = Codec.Crypto.RSA.verify (runPubKey pK) h s
--checkSignature pK s h = rsassa_pkcs1_v1_5_verify hashSHA1 (runPubKey pK) h s
checkSignature pK s h = trace "verify" $ C.verify hashDescrSHA1 (runPubKey pK) (B.toStrict h) (B.toStrict s)


sign :: PrivKey -> RawData -> RawData
--sign uK h = Codec.Crypto.RSA.sign (runPrivKey uK) h
sign uK = trace "sign" $ either (\_->B.empty) B.fromStrict . C.sign Nothing hashDescrSHA1 (runPrivKey uK) . B.toStrict

decrypt :: PrivKey -> RawData -> RawData
decrypt pK =  trace "decrpypt" $ either (\_-> B.empty) B.fromStrict . C.decrypt Nothing (runPrivKey pK) . B.toStrict

encrypt :: PubKey -> RawData -> IO RawData
encrypt pK d = do gen <- cprgCreate <$> createEntropyPool :: IO SystemRNG
                  print "encrypt"
                  pure . either (\_->B.empty) B.fromStrict . fst . C.encrypt gen (runPubKey pK) $B.toStrict d


generateKeyPair :: Int -> IO (PubKey,PrivKey)
generateKeyPair keySiz = do gen <- cprgCreate <$> createEntropyPool :: IO SystemRNG
                            print "generating key-pair"
                            let (pubK,privK) = fst $ generate gen (div keySiz 8) 65537
                            print $ "pubKey : " -- ++ show pubK
                            return $ (PubKey pubK, PrivKey privK)


