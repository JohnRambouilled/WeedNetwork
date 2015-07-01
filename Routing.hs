{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies #-}
module Routing where

import Control.Monad.State hiding (put, get)
import Control.Concurrent
import Data.Binary
import Data.List

import Class
import Crypto
import Packet
import Timer
import Log

data Routing = Routing {relayCallback :: RoutingCB,
                        destCallback :: RoutingCB}
type RoutingT = StateT Routing

type RoutingCB = Behaviour Routing Request RoutingAnswer
data RoutingAnswer = RoutingAnswer {routingTimeOut :: IO (),
                                    toRegister :: Maybe [DataCB],
                                    toRelay :: [Request]}

instance Modules Routing Request RoutingAnswer where
        onPacket req = if 1 + roadPosition req == roadLength req || roadPosition req == 0
                            then join $ gets destCallback <*> pure req
                            else join $ gets relayCallback <*> pure req


routingRelayCallback :: RoutingCB
routingRelayCallback req =  return [RoutingAnswer (pure ()) (Just relayFun) [req] ]
        where relayFun = [relayPipeMessage $ roadPosition req]

relayPipeMessage :: Int -> DataCB
relayPipeMessage n = DataCB hFun relayFun
        where hFun p@(DataPacket kH _ (DataContent cnt)) = case decodeMaybe cnt of
                                                Nothing -> do keepLog RoutingLog Error $ "[RELAY] Invalid data packet (decodeMaybe failed)"  
                                                              return Nothing
                                                Just pm@(PipeData n' b dat) -> do keepLog RoutingLog Normal $ "[RELAY] New pipe data : " ++ show pm ++ " " ++ show n
                                                                                  return $ if n == n' then Just $ (pipeMessageHash kH b dat, (relay p pm))
                                                                                                 else Nothing
                                                Just pm@(PipeExit n' b dat) -> if n == n' then do keepLog RoutingLog Important $ "[Relay] Closing Pipe : " ++ show kH
                                                                                                  unregisterKeyEntry kH
                                                                                                  return $ Just (pipeMessageHash kH b dat, (relay p pm))
                                                                                        else return Nothing
              hFun p@(Introduce kH _ _ (IntroContent cnt)) = case decodeMaybe cnt :: Maybe Request of
                                                             Nothing -> pure Nothing
                                                             Just req -> if roadPosition req == n then
                                                                           return $ Just (reqSourceHash kH req, 
                                                                                          p{introContent = IntroContent $ encode req{roadPosition = roadPosition req + 1}})
                                                                         else pure Nothing
              relay p pm = let pm' = pm{relayNumber = if (messageDirection pm) then 1 + relayNumber pm else (relayNumber pm) - 1 }
                               in p{datacontent = DataContent $ encode pm'}
              relayFun = pure . pure
pipeTimeOut :: DiffTime
pipeTimeOut = 200
pipeRefreshTO = 190 :: DiffTime



routingCryptoCallback :: MVar Crypto -> MVar Timer -> PubKey -> PrivKey -> SourceID -> MVar Routing -> CryptoCB Packet Packet
routingCryptoCallback cV tV pubK uK uID rV = genCryptoCallback cV tV pipeTimeOut rV inFun outFun
    where inFun :: HashFunction Request
          inFun (Introduce kH _ _ (IntroContent d)) = case decodeMaybe d of
                                      Just req ->  let n = roadPosition req in
                                                  do keepLog RoutingLog Important $ "New req  : \n" ++ show req
                                                     if checkRequest req then
                                                             if n == 0 && (checkSignature pubK (neighborSignature req) $ encryptedPrivKey req)
                                                               then return $ Just (reqSourceHash kH req, req)
                                                               else if ((road req) !! n) == uID 
                                                                      then do b <- cryptoCheckSig (keyHash $ road req !! (n-1)) (neighborSignature req) $ reqNeighHash kH req
                                                                              if b then return $ Just (reqSourceHash kH req, 
                                                                                                       req) 
                                                                                   else (keepLog RoutingLog Suspect $ "Bad Neighborg signature") >> pure Nothing
                                                                      else (keepLog RoutingLog Normal $ "Request not adressed to user") >> pure Nothing
                                                     else (keepLog RoutingLog Suspect $ "Non-conform request") >> pure Nothing
                                      Nothing -> pure Nothing
          inFun _ = return Nothing
          checkRequest (Request n _ l r _ _) = (l == length r) && (n < l) && (n >= 0)
          outFun :: Packet -> RoutingAnswer -> CryptoT IO (Maybe [CryptoAction], [Packet], IO())
          outFun p (RoutingAnswer onTO regM rL) = --do pktL <- liftIO $ forM rL (\r -> forgePacket p r <$> genRnd)
                                                     return (map runDataCB <$> regM, map (forgePacket p) rL, onTO)
          forgePacket p r = p{introContent = IntroContent . encode $ signReq (keyID p) r}
          signReq kH r = let r' = r{roadPosition = roadPosition r + 1} in r'{neighborSignature = sign uK pubK $ reqNeighHash kH r'}


reqNeighHash :: KeyHash -> Request -> Hash
reqNeighHash kH (Request n _ l r epk cnt) = encode (kH,n,l,r,epk,cnt)

reqSourceHash :: KeyHash -> Request -> Hash  
reqSourceHash kH (Request _ _ l r epk cnt) = encode (kH,l,r,epk,cnt)

pipeMessageHash :: KeyHash -> Bool -> RawData -> Hash
pipeMessageHash kH b d = encode (kH, b, d)




type Number = Int

data PipeMessage = PipeData {relayNumber :: Number,
                             messageDirection :: Bool,
                             messageContent :: RawData} |
                   PipeExit {relayNumber :: Number,
                             messageDirection :: Bool,
                             messageContent :: RawData} 
                               
instance Binary PipeMessage where
            put (PipeData n d cnt) = putWord8 0 >> put n >> put d >> put cnt
            put (PipeExit n d cnt) = putWord8 1 >> put n >> put d >> put cnt
            get = do i <- getWord8
                     case i of
                          0 -> PipeData <$> get <*> get <*> get
                          1 -> PipeExit <$> get <*> get <*> get
                          _ -> fail $ "Unable to parse PipeMessage" ++ show i

instance Show PipeMessage where
        show (PipeData n b _) = "PipeData : " ++ show n ++ " " ++ show b
        show (PipeExit n b _) = "PipeExit : " ++ show n ++ " " ++ show b

-- | Request Hash is : (Key, Length, Road, encryptKey, content)
data Request = Request {roadPosition :: Number, -- ^ Current position of the request
                        neighborSignature :: Sig, -- ^ Signature of the last relay (Key, Pos, Length, Road, encryptKey, content)
                        roadLength :: Number, -- ^ Total length of the road
                        road :: Road,  -- ^ Road : list of UserID
                        encryptedPrivKey :: RawData,  -- ^ encrypted (Keyhash, PrivKey) for the destination of the road
                        requestContent :: RawData}

instance Binary Request where put (Request n ns l r epk cnt) = put (0 :: Word8) >> put n >> put ns >> put l >> put r >> put epk >> put cnt
                              get = do n <- get :: Get Word8
                                       if n == 0 then Request <$> get <*> get <*> get <*> get <*> get <*> get
                                                 else fail "Not a Request"


instance Show Request where 
        show (Request n _ l r _ _) = "Request : " ++ "  n / l = " ++ show n ++ " / " ++ show l  ++ "   road = " ++ show r  


