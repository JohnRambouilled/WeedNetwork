module Packet where
import Data.List
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import qualified Data.ByteString as BStrct
import Data.ByteString.Lazy hiding (map, concat)
import Numeric(showHex)
--import Crypto.PubKey.RSA
--import Crypto.PubKey.ECC.DH
--import Crypto.PubKey.ECC.ECDSA
import qualified Crypto.PubKey.Ed25519 as S
import qualified Crypto.PubKey.Curve25519 as DH
import Crypto.Types.PubKey.ECC
import qualified Data.ByteArray as BA
import Crypto.Error

type SendFunction = Packet -> IO Bool

data Packet = Introduce   {keyID :: KeyHash, key :: PubKey,  sig :: Sig, introContent :: IntroContent} 
            | DataPacket  {keyID :: KeyHash, sig :: Sig, datacontent :: DataContent}

instance Binary Packet where
        put (Introduce kH k s cnt) = putWord8 0  >> put kH >> put k >> put s >> put cnt
        put (DataPacket kH s cnt) = putWord8 1 >> put kH >> put s >> put cnt
        get = do n <- getWord8
                 case n of
                   0 -> Introduce <$> get <*> get <*> get <*> get
                   1 -> DataPacket <$> get <*> get <*> get
                   _ -> fail "[Packet.hs] packet error"

instance Show Packet where show (Introduce kID _ _ _) = "IntroPacket from : " ++ show kID
                           show (DataPacket kID _ _ ) = "DataPacket  from : " ++ show kID


newtype IntroContent = IntroContent {runIntroContent :: RawData}
instance Binary IntroContent where put (IntroContent d) = putLazyByteString d
                                   get = IntroContent <$> getRemainingLazyByteString
newtype DataContent = DataContent {runDataContent :: RawData}
instance Binary DataContent where put (DataContent d) = putLazyByteString d
                                  get = DataContent <$> getRemainingLazyByteString

type RawData = ByteString 

type Sig = S.Signature


instance Binary S.Signature where put s = putByteString $ BA.convert s
                                  get = getCryptoFailable sigByteSize S.signature


dhPubKeyByteSize = 32 :: Int
instance Binary DH.PublicKey where put pk = putByteString $ BA.convert pk
                                   get = do b <- DH.publicKey <$> getByteString dhPubKeyByteSize
                                            case b of
                                                 Left e -> fail (show e) 
                                                 Right k -> pure k

--instance Binary Signature where put (Signature r s) = put r >> put s
--                                get = Signature <$> get <*> get

keyHashByteSize = 4 :: Int64
sigByteSize = 64 :: Int
keyByteSize = 32 :: Int

newtype KeyHash = KeyHash {runKeyHash :: RawData} deriving (Eq, Ord)
instance Binary KeyHash where put (KeyHash h) = putLazyByteString h
                              get = KeyHash <$> getLazyByteString keyHashByteSize
instance Show KeyHash where show (KeyHash d) = prettyPrint d


newtype PubKey = PubKey {runPubKey :: S.PublicKey} deriving (Show)
newtype PrivKey = PrivKey {runPrivKey :: S.SecretKey}
instance Binary PubKey where put (PubKey pk) = putByteString $ BA.convert pk
                             get = PubKey <$> getCryptoFailable keyByteSize S.publicKey 


instance Binary PrivKey where put (PrivKey pk) = putByteString $ BA.convert pk
                              get = PrivKey <$> getCryptoFailable keyByteSize S.secretKey 




type Road = [SourceID]
newtype SourceID = SourceID {keyHash :: KeyHash} deriving (Eq,Ord, Show)

instance Binary SourceID where put (SourceID i) = put i
                               get = SourceID <$> get



getCryptoFailable :: Int -> (BStrct.ByteString -> CryptoFailable a) -> Get a
getCryptoFailable n f = do b <- f <$> getByteString n
                           case b of CryptoFailed e -> fail (show e)
                                     CryptoPassed s -> pure s


decodeMaybe :: Binary a => ByteString -> Maybe a
decodeMaybe bs = case decodeOrFail bs of
                   Left _ -> Nothing
                   Right (_,_,a) -> Just a



isIntroduce (Introduce _ _ _ _) = True
isIntroduce _ = False

isDataPacket = not . isIntroduce

prettyPrint :: RawData -> String
prettyPrint = concat . map (flip showHex "") . unpack

