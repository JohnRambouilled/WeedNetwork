{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ExistentialQuantification #-}
module Crypto.Crypto where
import Control.Monad.State
import Data.Maybe
import qualified Data.ByteString as BStrct
import qualified Data.ByteString.Lazy as B
import Crypto.Random
import Crypto.PubKey.HashDescr hiding (HashFunction)
import qualified Crypto.PubKey.ECC.DH as E
import qualified Crypto.PubKey.ECC.ECDSA as E
import Crypto.Types.PubKey.ECC
import Crypto.PubKey.ECC.Generate

import Debug.Trace

import Class
import Packet
import Data.Binary
import Log
import Crypto.Module


hashSHA1 = hashFunction hashDescrSHA1 


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
   where computeHash x = let h = hashSHA1 $ trace ("computing hash of : " ) $ B.toStrict x
                         in B.take keyHashByteSize . B.reverse $ B.fromStrict $ trace ("hashlenght = " ++ (show $ BStrct.length h)) h
-- | Look for the Key in the map, and check the signature. Return False if the keyHash is unkown, or if the signature is not valid.
cryptoCheckSig :: (MonadIO m) => KeyHash -> Sig -> Hash -> CryptoT m Bool
cryptoCheckSig kH s h = do keepLog CryptoLog Normal $ "[cryptoCheckSig] :: checking signature for keyID : " ++ show kH
                           maybe (pure False) (\k -> pure $ checkSignature (pubKey k) s h) =<< (mapGetEntry kH) --return True

-- | Ckeck the validity of a signature.
checkSignature :: PubKey -> Sig -> Hash -> Bool
checkSignature pK s h = trace "verify" $ E.verify hashSHA1 (runPubKey pK) s $ B.toStrict h



sign :: CPRG g => g -> PrivKey -> RawData -> Sig
sign gen uK d = fst $ E.sign gen (runPrivKey uK) hashSHA1 $ B.toStrict d

type PubPoint = PublicPoint

transmitKey :: CPRG g => g -> PubKey -> (PubPoint, (PrivKey, PubKey))
transmitKey gen pK = (E.calculatePublic c privN, keysFromShared $ E.getShared c privN $ pkPublicPoint pK)
    where c = pkCurve pK
          cN = pkCurveName pK
          privN = fst $ E.generatePrivate gen c
          keysFromShared (E.SharedKey sk) = (PrivKey cN sk, PubKey cN $ E.calculatePublic c sk)


exctractKey :: PrivKey -> PubPoint -> (PrivKey, PubKey)
exctractKey prK pP = keysFromShared $ E.getShared c (prPrivateNumber prK) pP
    where c = prCurve prK
          cN = prCurveName prK
          keysFromShared (E.SharedKey sk) = (PrivKey cN sk, PubKey cN $ E.calculatePublic c sk)

    


generateKeyPair :: CPRG g => g -> CurveName -> (PubKey,PrivKey)
generateKeyPair gen cN = (PubKey cN (E.public_q pubK), PrivKey cN (E.private_d privK))
    where ((pubK, privK),_) = generate gen $ getCurveByName cN

genRnd :: IO SystemRNG 
genRnd = cprgCreate <$> createEntropyPool
