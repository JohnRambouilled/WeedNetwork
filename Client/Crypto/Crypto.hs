{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ExistentialQuantification #-}
module Client.Crypto.Crypto where
import Control.Monad.State
import Data.Maybe
import qualified Data.ByteString as BStrct
import qualified Data.ByteString.Lazy as B
import Crypto.Random
import Crypto.PubKey.HashDescr hiding (HashFunction)
import qualified Crypto.PubKey.Ed25519 as S
import qualified Crypto.PubKey.Curve25519 as DH
import Crypto.Error

import Client.Crypto.Module
import Client.Class
import Client.Packet
import Data.Binary
import Log


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
pubKeyToHash a = computeHash . encode $ a
   where computeHash x = let h = hashSHA1 $ B.toStrict x
                         in B.take keyHashByteSize . B.reverse $ B.fromStrict $ h
-- | Look for the Key in the map, and check the signature. Return False if the keyHash is unkown, or if the signature is not valid.
cryptoCheckSig :: (MonadIO m) => KeyHash -> Sig -> Hash -> CryptoT m Bool
cryptoCheckSig kH s h = do keepLog CryptoLog Normal $ "[cryptoCheckSig] :: checking signature for keyID : " ++ show kH
                           maybe (pure False) (\k -> pure $ checkSignature (pubKey k) s h) =<< (mapGetEntry kH) --return True

-- | Ckeck the validity of a signature.
checkSignature :: PubKey -> Sig -> Hash -> Bool
checkSignature pK s h = S.verify (runPubKey pK) (B.toStrict h) s 



sign :: PrivKey -> PubKey -> RawData -> Sig
sign uK pK d = S.sign  (runPrivKey uK) (runPubKey pK) $ B.toStrict d


-- | Dest pubKey, et new privKey to generate the pipeKey, and the pubKey to transmit to destinary (the new privKey can be discarded)
type DHPubKey = DH.PublicKey
type DHPrivKey = DH.SecretKey

transmitKey :: DHPubKey -> DHPrivKey -> Maybe (DHPubKey, (PrivKey, PubKey))
transmitKey dK nK = (\keys -> (DH.toPublic nK, keys)) <$> (keysFromShared $ DH.dh dK nK)
    where keysFromShared dhS = let sKM = S.secretKey dhS
                  in case sKM of CryptoPassed sK -> Just (PrivKey sK, PubKey $ S.toPublic sK )
                                 _ -> Nothing

exctractKey :: DHPubKey -> DHPrivKey -> Maybe (PrivKey, PubKey)
exctractKey pK prK = keysFromShared $ DH.dh pK prK
    where keysFromShared dhS = let sKM = S.secretKey dhS
                  in case sKM of CryptoPassed sk -> Just (PrivKey sk, PubKey $ S.toPublic sk )
                                 _ -> Nothing
    


generateKeyPair ::  IO (PubKey,PrivKey)
generateKeyPair = do skBs <- getRandomBytes 32 :: IO BStrct.ByteString
                     case S.secretKey skBs of
                                CryptoFailed e -> fail (show e)
                                CryptoPassed k -> let pK = S.toPublic k in pure (PubKey pK, PrivKey k)
        


generateDHPrivKey :: IO DHPrivKey
generateDHPrivKey = do skBs <- getRandomBytes 32 :: IO BStrct.ByteString
                       case DH.secretKey skBs of
                                Left e -> fail (show e)
                                Right k -> pure k
        

privKeyToDHPrivKey :: PrivKey -> Maybe DHPrivKey
privKeyToDHPrivKey (PrivKey pk) = case DH.secretKey pk of Left e -> Nothing
                                                          Right k -> Just k

privKeyToDHPubKey :: PrivKey -> Maybe DHPubKey
privKeyToDHPubKey = (DH.toPublic <$>) . privKeyToDHPrivKey



