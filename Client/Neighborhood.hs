{-# LANGUAGE MultiParamTypeClasses,FlexibleContexts #-}
module Client.Neighborhood where

import Client.Class
import Client.Crypto
import Client.Packet
import Control.Monad.State hiding (put,get)
import Data.Binary
import Control.Concurrent.MVar
import Data.ByteString.Lazy hiding (concat,map)

import Timer


data NeighHello = NeighHello RawData
                | NeighClose 

data Neighborhood = Neighborhood {nhCallbacks :: [DataCB]}
newtype NeighRet = NeighRet {runNeighRet :: MVar Neighborhood -> CryptoAction}

instance Modules Neighborhood NeighHello NeighRet where 
        onPacket h = pure $ (:[]) $ NeighRet $ registeredCB
             where registeredCB :: MVar Neighborhood -> CryptoAction
                   registeredCB mvar kE p = do val <- (liftIO $ (readMVar mvar :: IO Neighborhood))
                                               concat <$> sequence (map (($p).($kE).runDataCB) $ nhCallbacks val) 

cryptoTTLmax = 250 :: DiffTime --TODO

registerNeighHelloCB :: (MonadIO m) => DataCB -> StateT Neighborhood m ()
registerNeighHelloCB x = modify $ \s -> s {nhCallbacks = x:nhCallbacks s}
                                       

neighCryptoCallback :: MVar Crypto -> MVar Timer -> MVar Neighborhood -> CryptoCB Packet Packet
neighCryptoCallback crypto timer mv = genCryptoCallback crypto timer cryptoTTLmax mv inFun oFun
  where inFun :: HashFunction NeighHello
        inFun (Introduce kH k _ (IntroContent cnt)) = maybe (pure Nothing) (\nH -> case nH of
                                                                    NeighHello _  -> pure $ Just (encode (kH,k,cnt), nH)
                                                                    NeighClose -> unregisterKeyEntry kH >> return Nothing) (decodeMaybe cnt)
        oFun _ r = return (Just [runNeighRet r $ mv], [], pure ())


instance Binary NeighHello where
        put (NeighHello r) = putWord8 1 >> put r
        put NeighClose = putWord8 2
        get = do r <- getWord8 
                 case r of
                   1 -> NeighHello <$> get
                   2 -> pure NeighClose
                   _ -> fail ""

       

