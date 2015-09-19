{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Crypto where
import Data.ByteString.Lazy hiding (split)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
--import Reactive.Banana.Combinators
--import Reactive.Banana.Frameworks
--import Reactive.Banana.Switch
--import Control.Event.Handler
import FRP.Sodium
import Class
import FRP.Sodium.Internal hiding (Event)

type EventSource a = (Event a, Handler a)

data KeyHash = KeyHash Int deriving (Eq, Ord)
type PubKey = Int
type PrivKey = Int
type Signature = Int
type RawData = ByteString

class Signed a where hash :: a -> RawData
                     keyHash :: a -> KeyHash
                     signature :: a -> Signature

instance Signed a => IDable a KeyHash where extractID = keyHash

class Signed a => IntroClass a where introKey :: a -> PubKey


buildCryptoMap :: (IntroClass i, Signed e) => Event i -> Event KeyHash -> Event e -> Reactive (Behavior (EventMap KeyHash e))
buildCryptoMap introE decoE packetE = do eM <- accum (EventMap M.empty) $ merge (deleteKey <$> decoE) (execute (insertCryptoKey <$> introE))
                                         listenTrans (packetActions eM) id
                                         pure eM
    where insertCryptoKey intro = do --(event, fire) <- newEvent
                                     --return $ insertEntry (keyHash intro) $ EventMapEntry fire event (checkSig $ introKey intro)
                                     entry <- newEventEntry (checkSig $ introKey intro)
                                     pure $ insertEntry (keyHash intro) entry 
          packetActions eM = filterJust $ snapshot (flip fireKey) packetE eM



checkSig :: Signed a => PubKey -> a -> Bool
checkSig _ _  = True
sign :: Signed a => PrivKey -> a -> Signature
sign _ _ = 0
decrypt _ = id
computeHashFromKey _ = 0



isLeft (Left _) = True
isLeft _ = False
isRight = not . isLeft


swapB :: (Ord k, Eq k) => Behaviour (Reactive (M.Map k a)) -> Reactive (Behaviour (M.Map k a))
swapB mapB = hold M.empty $ execute $ value mapB

