module Client.Crypto (module Types.Crypto,
                      module Client.Crypto)
where


import Types
import Types.Crypto

import Data.Binary
import qualified Crypto.PubKey.Ed25519 as S
import qualified Crypto.PubKey.Curve25519 as DH
import qualified Crypto.Hash as H
import qualified Crypto.Error as E
import qualified Crypto.Random as R
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BStrct
import qualified Data.ByteString.Lazy as B


-- | Check the signature of a Signed object for the given public key. 
checkSig :: SignedClass a => PubKey -> a -> Bool
checkSig pK a  = checkSignature (scSignature a) $ scHash a
    where checkSignature s h = S.verify (sigPubKey pK) (B.toStrict h) s 

-- | signe a signable object with the given key pair
sign :: SignedClass a => KeyPair-> a -> a
sign (pK, k) a = scPushSignature a . makeSignature $ scHash a
    where makeSignature d = S.sign  (sigPrivKey k) (sigPubKey pK) $ B.toStrict d

-- | Create a KeyHash from a key
computeHashFromKey :: PubKey -> KeyHash
computeHashFromKey = KeyHash . computeHash

--computeHashFromPipeKey :: PipePubKey -> PipeKeyHash
--computeHashFromPipeKey = PipeKeyHash . computeHash

computePipeID :: Road -> PipeID
computePipeID = PipeID . computeHash

-- | Generate a shared pipeKeyPair using your private key and your destinary's public key.
-- | The private key of this pipe is a secret shared by only you both,
-- | and can be used to communicate privately.
genPipeKeys :: PubKey -> PrivKey -> Maybe PipeKeyPair
genPipeKeys pK sK = case S.secretKey $ DH.dh (dhPubKey pK) (dhPrivKey sK) of
                        E.CryptoFailed e -> Nothing
                        E.CryptoPassed s -> Just (PipePubKey $ S.toPublic s, PipePrivKey s)


-- | Generate (pseudo) randomly a key pair, containing both Diffie-Hellman public and secret key,
-- | and signing keys (both from curve Ed25519)
generateKeyPair :: IO (PubKey,PrivKey)
generateKeyPair = do (skBs,_) <- R.randomBytesGenerate sigPrivKeyByteSize <$> R.getSystemDRG
                     sK <- E.throwCryptoErrorIO $ S.secretKey (skBs :: BStrct.ByteString)
                     (dhSkBs,_) <- R.randomBytesGenerate dhPrivKeyByteSize <$> R.getSystemDRG
                     dhSk <- E.throwCryptoErrorIO $ DH.secretKey (dhSkBs :: BStrct.ByteString)
                     pure (PubKey (S.toPublic sK) (DH.toPublic dhSk), PrivKey sK dhSk)

-- | Compute the hash of a Binary object.
computeHash :: (Show a, Binary a) => a -> Hash
computeHash a = let digest = H.hashWith hashAlgorithm (B.toStrict $ encode a) 
                in B.take (fromIntegral keyHashByteSize) . B.reverse . B.fromStrict . BA.convert $ digest

