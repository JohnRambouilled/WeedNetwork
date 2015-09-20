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
type Payload = RawData

class SignedClass a where scHash :: a -> RawData
                          scKeyHash :: a -> KeyHash
                          scSignature :: a -> Signature

instance SignedClass a => IDable a KeyHash where extractID = scKeyHash

class SignedClass a => IntroClass a where icPubKey :: a -> PubKey



buildCryptoMap :: (IntroClass i, SignedClass e) => Event i -> Event e -> Reactive (EventEntryMapBhv KeyHash e, Event (i, (EventEntry e)))
buildCryptoMap introE packetE = let newEntryE = execute (insertCryptoKey <$> introE) in
                                      do --eM <- accum M.empty $ merge (deleteKey <$> decoE) (fst <$> newEntryE)
                                         (eM, orderH) <- newBhvTpl M.empty --creation de la Map
                                         listenTrans (fst <$> newEntryE) orderH --On ajoute les nouvelles entrées
                                         listenTrans (packetActions eM) id    --On fire les packets dans la map
                                         pure ((eM, orderH), snd <$> newEntryE)
    where insertCryptoKey intro = do entry <- newEventEntry (checkSig $ icPubKey intro)
                                     pure (insertEntry (scKeyHash intro) entry, (intro, entry))
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

fromLeft (Left x) = x
fromLeft _ = error $ "fromLeft on Right"

fromRight (Right x) = x
fromRight _ = error $ "fromRight on Left"



