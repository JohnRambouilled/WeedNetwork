module Packet where
import Data.List
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.ByteString.Lazy hiding (map, concat)
import Numeric(showHex)
--import Crypto.PubKey.RSA
import Crypto.PubKey.ECC.DH
import Crypto.PubKey.ECC.ECDSA
import Crypto.Types.PubKey.ECC

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

type Sig = Signature
instance Binary Signature where put (Signature r s) = put r >> put s
                                get = Signature <$> get <*> get

keyHashByteSize = 4 :: Int64
sigByteSize = 128 :: Int64

newtype KeyHash = KeyHash {runKeyHash :: RawData} deriving (Eq, Ord)
instance Binary KeyHash where put (KeyHash h) = putLazyByteString h
                              get = KeyHash <$> getLazyByteString keyHashByteSize
instance Show KeyHash where show (KeyHash d) = prettyPrint d


data PubKey = PubKey {pkCurveName :: CurveName,
                      pkPublicPoint :: PublicPoint} deriving Show
instance Binary PubKey where put (PubKey c p) = put (fromEnum c) >> put p
                             get = PubKey <$> (toEnum <$> get ) <*> get
pkCurve :: PubKey -> Curve
pkCurve = getCurveByName . pkCurveName
runPubKey :: PubKey -> PublicKey
runPubKey (PubKey cN pP) = PublicKey (getCurveByName cN) pP

instance Binary Point where
        put PointO = put False
        put (Point x y) = put True >> put x >> put y
        get = do b <- get :: Get Bool
                 if b then Point <$> get <*> get
                      else pure PointO


--instance Binary PublicKey where put (PublicKey size n e) = put size >> put n >> put e
--                                get = PublicKey <$> get <*> get <*> get


data PrivKey = PrivKey {prCurveName :: CurveName,
                        prPrivateNumber :: PrivateNumber} deriving Show
instance Binary PrivKey where put (PrivKey c n) = put (fromEnum c) >> put n
                              get = PrivKey <$> (toEnum <$> get) <*> get

prCurve :: PrivKey -> Curve
prCurve = getCurveByName . prCurveName 
runPrivKey :: PrivKey -> PrivateKey
runPrivKey (PrivKey cN pN) = PrivateKey (getCurveByName cN) pN




type Road = [SourceID]
newtype SourceID = SourceID {keyHash :: KeyHash} deriving (Eq,Ord, Show)

instance Binary SourceID where put (SourceID i) = put i
                               get = SourceID <$> get

decodeMaybe :: Binary a => ByteString -> Maybe a
decodeMaybe bs = case decodeOrFail bs of
                   Left _ -> Nothing
                   Right (_,_,a) -> Just a



isIntroduce (Introduce _ _ _ _) = True
isIntroduce _ = False

isDataPacket = not . isIntroduce

prettyPrint :: RawData -> String
prettyPrint = concat . map (flip showHex "") . unpack

