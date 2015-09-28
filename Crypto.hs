{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, FlexibleInstances, FunctionalDependencies #-}
module Crypto (module Crypto,
               module Ed25519)
where
import Data.ByteString.Lazy hiding (split)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import FRP.Sodium
import FRP.Sodium.Internal hiding (Event)
import GHC.Generics

import Class
import Ed25519

type EventSource a = (Event a, Handler a)

newtype KeyHash = KeyHash RawData deriving (Eq, Ord, Generic)
instance Binary KeyHash
type Payload = RawData

class SignedClass a where scHash :: a -> RawData
                          scKeyHash :: a -> KeyHash
                          scSignature :: a -> Signature
                          scPushSignature :: a -> Signature -> a

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
checkSig k a  = checkSignature k (scSignature a) $ scHash a

sign :: SignedClass a => KeyPair-> a -> a
sign (pK, k) a = scPushSignature a $ makeSignature k pK $ scHash a

decryptPrivKey :: DHPubKey -> PrivKey -> Maybe KeyPair
decryptPrivKey dh k = privKeyToDHPrivKey k >>= exctractKey dh

computeHashFromKey :: PubKey -> KeyHash
computeHashFromKey = KeyHash . computeHash



isLeft (Left _) = True
isLeft _ = False
isRight = not . isLeft

fromLeft (Left x) = x
fromLeft _ = error $ "fromLeft on Right"

fromRight (Right x) = x
fromRight _ = error $ "fromRight on Left"



