{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, FlexibleInstances, FunctionalDependencies #-}
module Crypto where
import Data.ByteString.Lazy hiding (split)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import FRP.Sodium
import Class
import FRP.Sodium.Internal hiding (Event)
import GHC.Generics

type EventSource a = (Event a, Handler a)

newtype KeyHash = KeyHash Int deriving (Eq, Ord, Generic)
instance Binary KeyHash
type PubKey = Int
type PrivKey = Int
type Signature = Int
type RawData = ByteString

class SignedClass a where scHash :: a -> RawData
                          scKeyHash :: a -> KeyHash
                          scSignature :: a -> Signature

instance SignedClass a => IDable a KeyHash where extractID = scKeyHash

class SignedClass a => IntroClass a where icPubKey :: a -> PubKey


buildCryptoMap :: (IntroClass i, SignedClass e) => Event i -> Event KeyHash -> Event e -> Reactive (Behavior (EventMap KeyHash e))
buildCryptoMap introE decoE packetE = do eM <- accum (EventMap M.empty) $ merge (deleteKey <$> decoE) (execute (insertCryptoKey <$> introE))
                                         listenTrans (packetActions eM) id
                                         pure eM
    where insertCryptoKey intro = insertEntry (scKeyHash intro) <$> newEventEntry (checkSig $ icPubKey intro)
          packetActions eM = filterJust $ snapshot (flip fireKey) packetE eM



checkSig :: SignedClass a => PubKey -> a -> Bool
checkSig _ _  = True
sign :: SignedClass a => PrivKey -> a -> Signature
sign _ _ = 0
decrypt _ = id
computeHashFromKey _ = 0



isLeft (Left _) = True
isLeft _ = False
isRight = not . isLeft


swapB :: (Ord k, Eq k) => Behaviour (Reactive (M.Map k a)) -> Reactive (Behaviour (M.Map k a))
swapB mapB = hold M.empty $ execute $ value mapB

