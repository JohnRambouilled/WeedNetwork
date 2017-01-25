module Client.Crypto (module Types.Crypto,
                      module Client.Crypto)
where


import Types.Crypto
import qualified Crypto.PubKey.Ed25519 as S
import qualified Crypto.PubKey.Curve25519 as DH
import Crypto.Hash
import Crypto.Error
import Crypto.Random
import Data.Binary
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BStrct
import qualified Data.ByteString.Lazy as B



checkSig :: SignedClass a => PubKey -> a -> Bool
checkSig k a  = checkSignature k (scSignature a) $ scHash a

sign :: SignedClass a => KeyPair-> a -> a
sign (pK, k) a = scPushSignature a $ makeSignature k pK $ scHash a


computeHashFromKey :: PubKey -> KeyHash
computeHashFromKey = KeyHash . computeHash


checkSignature :: PubKey -> Signature -> Hash -> Bool
checkSignature pK s h = S.verify (sigPubKey pK) (B.toStrict h) s 

makeSignature :: PrivKey -> PubKey -> RawData -> Signature
makeSignature uK pK d = S.sign  (sigPrivKey uK) (sigPubKey pK) $ B.toStrict d


genPipeKeys :: PubKey -> PrivKey -> Maybe PipeKeyPair
genPipeKeys pK sK = case S.secretKey $ DH.dh (dhPubKey pK) (dhPrivKey sK) of
                        CryptoFailed e -> Nothing
                        CryptoPassed s -> Just (PipePubKey $ S.toPublic s, PipePrivKey s)
    

{-
transmitKey :: DHPubKey -> DHPrivKey -> Maybe (DHPubKey, KeyPair)
transmitKey dK nK = (\keys -> (DH.toPublic nK, keys)) <$> (keysFromShared $ DH.dh dK nK)
    where keysFromShared dhS = let sKM = S.secretKey dhS
                      in case sKM of CryptoPassed sK -> Just (PubKey $ S.toPublic sK, PrivKey sK )
                                     _ -> Nothing
decryptKeyPair :: DHPubKey -> DHPrivKey -> Maybe KeyPair
decryptKeyPair pK prK = keysFromShared $ DH.dh pK prK
    where keysFromShared dhS = let sKM = S.secretKey dhS
                               in case sKM of CryptoPassed sk ->  Just (PubKey $ S.toPublic sk, PrivKey sk)
                                              _ -> Nothing
-} 
                                                                                                                  

generateKeyPair :: IO (PubKey,PrivKey)
generateKeyPair = do (skBs,_) <- randomBytesGenerate sigPrivKeyByteSize <$> getSystemDRG
                     sK <- throwCryptoErrorIO $ S.secretKey (skBs :: BStrct.ByteString)
                     (dhSkBs,_) <- randomBytesGenerate dhPrivKeyByteSize <$> getSystemDRG
                     dhSk <- throwCryptoErrorIO $ DH.secretKey (dhSkBs :: BStrct.ByteString)
                     pure (PubKey (S.toPublic sK) (DH.toPublic dhSk), PrivKey sK dhSk)


{-
generateDHKeyPair :: IO (DHPubKey, DHPrivKey)
generateDHKeyPair  = do (skBs,_) <- randomBytesGenerate keyByteSize <$> getSystemDRG
                        sK <- throwCryptoErrorIO $ DH.secretKey (skBs :: BStrct.ByteString)
                        pure (DH.toPublic sK, sK)
                 


privKeyToPubKeyDH :: DHPrivKey -> DHPubKey
privKeyToPubKeyDH = DH.toPublic
-}

computeHash :: (Show a, Binary a) => a -> Hash
computeHash a = let digest = hash (B.toStrict $ encode a) :: Digest SHA1
                in B.take keyHashByteSize . B.reverse . B.fromStrict . BA.convert $ digest


