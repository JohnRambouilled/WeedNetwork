module Ed25519 where


--import Crypto.PubKey.HashDescr hiding (HashFunction)
import qualified Crypto.PubKey.Ed25519 as S
import qualified Crypto.PubKey.Curve25519 as DH
import Crypto.Hash
import Crypto.Error
import Crypto.Random
import Data.Int
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Numeric(showHex)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BStrct
import qualified Data.ByteString.Lazy as B

type RawData = B.ByteString 
type Hash = RawData

type Signature = S.Signature
emptySignature :: S.Signature
emptySignature = throwCryptoError . S.signature $ BStrct.replicate 64 0 

newtype PubKey = PubKey {runPubKey :: S.PublicKey} deriving (Show)
newtype PrivKey = PrivKey {runPrivKey :: S.SecretKey}
type KeyPair = (PubKey, PrivKey)

dhPubKeyByteSize = 32 :: Int
keyHashByteSize = 4 :: Int64
sigByteSize = 64 :: Int
keyByteSize = 32 :: Int

checkSignature :: PubKey -> Signature -> Hash -> Bool
checkSignature pK s h = S.verify (runPubKey pK) (B.toStrict h) s 

makeSignature :: PrivKey -> PubKey -> RawData -> Signature
makeSignature uK pK d = S.sign  (runPrivKey uK) (runPubKey pK) $ B.toStrict d

type DHPubKey = DH.PublicKey
type DHPrivKey = DH.SecretKey

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
                                                                                                                  

generateKeyPair :: IO (PubKey,PrivKey)
generateKeyPair = do (skBs,_) <- randomBytesGenerate keyByteSize <$> getSystemDRG
                     sK <- throwCryptoErrorIO $ S.secretKey (skBs :: BStrct.ByteString)
                     pure ( PubKey $ S.toPublic sK, PrivKey sK)



generateDHKeyPair :: IO (DHPubKey, DHPrivKey)
generateDHKeyPair  = do (skBs,_) <- randomBytesGenerate keyByteSize <$> getSystemDRG
                        case DH.secretKey (skBs :: BStrct.ByteString) of
                                Left s -> fail s
                                Right k -> pure (DH.toPublic k, k)
                 

--privKeyToDHPrivKey :: PrivKey -> Maybe DHPrivKey
--privKeyToDHPrivKey (PrivKey pk) = case DH.secretKey pk of Left e -> Nothing
--                                                          Right k -> Just k

privKeyToPubKeyDH :: DHPrivKey -> DHPubKey
privKeyToPubKeyDH = DH.toPublic


computeHash :: (Show a, Binary a) => a -> Hash
computeHash a = let digest = hash (B.toStrict $ encode a) :: Digest SHA1
                in B.take keyHashByteSize . B.reverse . B.fromStrict . BA.convert $ digest





getCryptoFailable :: Int -> (BStrct.ByteString -> CryptoFailable a) -> Get a
getCryptoFailable n f = do b <- f <$> getByteString n
                           case b of CryptoFailed e -> fail (show e)
                                     CryptoPassed s -> pure s


instance Binary S.Signature where put s = putByteString $ BA.convert s
                                  get = getCryptoFailable sigByteSize S.signature

instance Binary PubKey where put (PubKey pk) = putByteString $ BA.convert pk
                             get = PubKey <$> getCryptoFailable keyByteSize S.publicKey 

instance Binary PrivKey where put (PrivKey pk) = putByteString $ BA.convert pk
                              get = PrivKey <$> getCryptoFailable keyByteSize S.secretKey 

instance Binary DH.PublicKey where put pk = putByteString $ BA.convert pk
                                   get = do b <- DH.publicKey <$> getByteString dhPubKeyByteSize
                                            case b of
                                                    Left e -> fail (show e) 
                                                    Right k -> pure k

prettyPrint :: RawData -> String
prettyPrint = concat . map (flip showHex "") . B.unpack

