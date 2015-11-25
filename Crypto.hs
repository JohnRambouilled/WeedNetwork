{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, FlexibleInstances, FunctionalDependencies #-}
module Crypto (module Crypto,
               module Ed25519)
where
import Data.ByteString.Lazy hiding (split)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import GHC.Generics
import Reactive.Banana
import Reactive.Banana.Frameworks

import Timer
import Class
import Ed25519


newtype KeyHash = KeyHash RawData deriving (Eq, Ord, Generic)
type Payload = RawData

emptyPayload = Data.ByteString.Lazy.empty :: Payload

{- | Classe de type des packets signés -}
class SignedClass a where scHash :: a -> RawData    -- ^ Hash du packet (utilisé pour signer, et vérifier les signatures)
                          scKeyHash :: a -> KeyHash     -- ^ KeyHash de la clef publique utilisée pour signer le packet
                          scSignature :: a -> Signature  -- ^ Signature du packet
                          scPushSignature :: a -> Signature -> a   -- ^ fonction de remplacement de la signature (permet de signer)

instance SignedClass a => IDable a KeyHash where extractID = scKeyHash  -- ^ Instance de IDable des packets signés (identifiés par le KeyHash)


{- | Classe de type des packets introduisant une clef : il s'agit de packets signés, contenant de plus une clef publique -}
class SignedClass a => IntroClass a where icPubKey :: a -> PubKey


type CryptoMap t e = BehaviorC t (M.Map KeyHash (EventC e))  -- ^ map des events de packets signés, triés par keyHash
type CryptoEvent t i e = Event t (Either i (i, EventC e))    -- ^ Event de nouvelles connexions

{- | Construction de la cryptoMap a partir d'un Event d'introduce, et d'un Event de packets. 
 -   Les signatures des packets sont vérifiées. Renvoi la Map des events, l'Event de nouvelles connexions  
 -   [TODO] : gestion des refresh? -}
buildCryptoMap :: (Frameworks t, IntroClass i, SignedClass e) => Event t i -> Event t e -> Moment t (CryptoMap t e, CryptoEvent t i e)
buildCryptoMap introE packetE = do (cryptoE, cryptoH) <- newEvent
                                   (buildE, buildH) <- newEvent
                                   cMap <- buildEventCMapWith buildE
                                   reactimate $ apply (onIntro cryptoH buildH <$> bmLastValue cMap) $ filterE checkIntro introE
                                   fireKeyBhv (bmLastValue cMap) packetE
                                   pure ((eEventC <$>) <$> bmBhvC cMap, cryptoE)
    where makeCloseEvent i = do (h,ce) <- newEventC
                                let ce' = ce{ceAddHandler = filterSig i $ ceAddHandler ce}
                                pure (i, EventEntry h ce)
          filterSig i = filterIO (pure . checkSig (icPubKey i))
          onIntro :: (IntroClass i, SignedClass e) => Handler (Either i (i, EventC e)) -> Handler ((KeyHash, EventC e), EventEntry e) -> M.Map KeyHash (EventEntry e) -> i -> IO ()
          onIntro ch bh m i = case scKeyHash i `M.lookup` m of 
                                  Just _ -> ch $ Left i
                                  Nothing -> do (i, ee) <- makeCloseEvent i
                                                bh ((scKeyHash i, eEventC ee), ee)
                                                ch $ Right (i, eEventC ee)
                                
          checkIntro i = checkSig (icPubKey i) i -- && computeHashFromKey (icPubKey i) == scKeyHash i 

checkSig :: SignedClass a => PubKey -> a -> Bool
checkSig k a  = checkSignature k (scSignature a) $ scHash a

sign :: SignedClass a => KeyPair-> a -> a
sign (pK, k) a = scPushSignature a $ makeSignature k pK $ scHash a


computeHashFromKey :: PubKey -> KeyHash
computeHashFromKey = KeyHash . computeHash



isLeft (Left _) = True
isLeft _ = False
isRight = not . isLeft

fromLeft (Left x) = x
fromLeft _ = error $ "fromLeft on Right"

fromRight (Right x) = x
fromRight _ = error $ "fromRight on Left"

instance Show KeyHash where show (KeyHash d) = prettyPrint d
instance Binary KeyHash

