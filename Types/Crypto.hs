{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, FlexibleInstances #-}
module Types.Crypto where

import Data.Binary
import GHC.Generics
import qualified Crypto.PubKey.Ed25519 as S
import qualified Crypto.PubKey.Curve25519 as DH
import qualified Crypto.Hash as H
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BStrct
import qualified Data.ByteString.Lazy as B
import Crypto.Error
import Data.Int
import Numeric(showHex)
import Data.Binary.Get
import Data.Binary.Put

    -- ### CONSTANTES ###
dhPubKeyByteSize = 32 :: Int
dhPrivKeyByteSize = 32 :: Int
sigPubKeyByteSize = 32 :: Int
sigPrivKeyByteSize = 32 :: Int
sigByteSize = 64 :: Int
hashAlgorithm = H.SHA256
keyHashByteSize = H.hashDigestSize hashAlgorithm :: Int


    -- ### NEWTYPES ###
type RawData = B.ByteString 
type Payload = RawData
emptyPayload = B.empty :: Payload
type Hash = RawData
newtype KeyHash = KeyHash RawData deriving (Eq, Ord, Generic)
--newtype PipeKeyHash = PipeKeyHash RawData deriving (Eq, Ord, Generic)
type Signature = S.Signature
emptySignature :: S.Signature
emptySignature = throwCryptoError . S.signature $ BStrct.replicate 64 0 

    -- ### PIPES KEYS ###
newtype PipePubKey = PipePubKey {pipePubKey :: S.PublicKey} deriving (Show, Generic)
newtype PipePrivKey = PipePrivKey {pipePrivKey :: S.SecretKey}
type PipeKeyPair = (PipePubKey, PipePrivKey)

    -- ### USERS KEYS ###
    -- including signatures keys and Diffie-Hellman keys
data PubKey = PubKey {sigPubKey :: S.PublicKey,
                      dhPubKey :: DH.PublicKey} deriving (Show, Generic, Eq)
data PrivKey = PrivKey {sigPrivKey :: S.SecretKey,
                        dhPrivKey :: DH.SecretKey}
type KeyPair = (PubKey, PrivKey)

{- | Classe de type des packets signés -}
class SignedClass a where scHash :: a -> RawData    -- ^ Hash du packet (utilisé pour signer, et vérifier les signatures)
                          scSignature :: a -> Signature  -- ^ Signature du packet
                          scPushSignature :: a -> Signature -> a   -- ^ fonction de remplacement de la signature (permet de signer)

{- | Classe de type des packets introduisant une clef : il s'agit de packets signés, contenant de plus une clef publique -}
class SignedClass a => IntroClass a where icPubKey :: a -> PubKey

instance Binary PipePubKey
instance Binary PubKey
instance Show KeyHash where show (KeyHash d) = prettyPrint d
instance Binary KeyHash
--instance Binary PipeKeyHash

prettyPrint :: RawData -> String
prettyPrint = concatMap (`showHex` "") . B.unpack

instance Binary S.Signature where put s = putByteString $ BA.convert s
                                  get = getCryptoFailable sigByteSize S.signature

instance Binary S.PublicKey where put pk = putByteString $ BA.convert pk
                                  get = getCryptoFailable sigPubKeyByteSize S.publicKey 


instance Binary DH.PublicKey where put pk = putByteString $ BA.convert pk
                                   get = getCryptoFailable dhPubKeyByteSize DH.publicKey

getCryptoFailable :: Int -> (BStrct.ByteString -> CryptoFailable a) -> Get a
getCryptoFailable n f = do b <- f <$> getByteString n
                           case b of CryptoFailed e -> fail (show e)
                                     CryptoPassed s -> pure s


