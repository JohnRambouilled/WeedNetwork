module Packet where
import Data.List
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.ByteString.Lazy hiding (map, concat)
import Numeric(showHex)
import Crypto.PubKey.RSA

type SendFunction = Packet -> IO Bool

data Packet = Introduce   {keyID :: KeyHash, key :: PubKey,  sig :: Sig, introContent :: IntroContent} 
            | DataPacket  {keyID :: KeyHash, sig :: Sig, datacontent :: DataContent}

instance Binary Packet where
        put (Introduce kH k s cnt) = putWord8 0  >> put kH >> put k >> putLazyByteString s >> put cnt
        put (DataPacket kH s cnt) = putWord8 1 >> put kH >> putLazyByteString s >> put cnt
        get = do n <- getWord8
                 case n of
                   0 -> Introduce <$> get <*> get <*> getLazyByteString sigByteSize <*> get
                   1 -> DataPacket <$> get <*> getLazyByteString sigByteSize <*> get
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

type Sig = RawData

keyHashByteSize = 4 :: Int64
sigByteSize = 128 :: Int64

newtype KeyHash = KeyHash {runKeyHash :: RawData} deriving (Eq, Ord)
instance Binary KeyHash where put (KeyHash h) = putLazyByteString h
                              get = KeyHash <$> getLazyByteString keyHashByteSize
instance Show KeyHash where show (KeyHash d) = prettyPrint d


newtype PubKey = PubKey {runPubKey :: PublicKey}
instance Binary PubKey where put (PubKey k) = put  k
                             get = PubKey <$> get

instance Binary PublicKey where put (PublicKey size n e) = put size >> put n >> put e
                                get = PublicKey <$> get <*> get <*> get


newtype PrivKey = PrivKey {runPrivKey :: PrivateKey}
instance Binary PrivKey where put (PrivKey k) = put  k
                              get = PrivKey <$> get

instance Binary PrivateKey where 
        put (PrivateKey pub d _ _ _ _ _) = put pub >> put d
        get = do (pub, d) <- (,) <$> get <*> get
                 pure $ PrivateKey pub d 0 0 0 0 0






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

