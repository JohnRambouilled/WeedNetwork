{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies #-}
module Client.Pipes where

import Control.Monad.State hiding (put, get)
import qualified Control.Monad.State  as S (get)
import Control.Concurrent
import Data.Binary 
import Data.List
import Data.Maybe
import qualified Data.Map as M

import Client.Class
import Client.Crypto
import Client.Packet
import Client.Routing
import Client.Ressource
import Data.Time hiding (DiffTime)
import Timer
import Log

newtype RoadID = RoadID RawData deriving (Eq, Ord)
instance Show RoadID where show (RoadID d) = prettyPrint d

data Pipes  = Pipes {pipesMap :: M.Map RoadID PipeEntry,
                     pipesList :: [RoadID]}

data PipeEntry = PipesEntry {roadSpecs :: RoadSpecs,
                             writeFun :: ComMessage -> IO Bool,
                             breakFun :: RawData -> IO Bool}

data RoadSpecs =  RoadSpecs {roadID :: RoadID,
                             roadLength :: Int,
                             roadPing :: DiffTime,
                             roadOpenedCom :: Int,
                             roadInTraffic :: Int,
                             roadOutTraffic :: Int}



genRoadSpecs :: Time -> Road -> IO RoadSpecs
genRoadSpecs t0 r = do t <- getTime 
                       pure $ RoadSpecs (roadToRoadID r) (length r) (diffUTCTime t t0) 0 0 0


genNewPipeRequest :: MVar RandomGen -> PrivKey -> PubKey -> DHPubKey -> Road -> RawData -> IO (PrivKey, PubKey, Packet)
genNewPipeRequest gV uK pK destK r d = do dhP <- generateDHPrivKey gV
                                          t <- getTime
                                          let (pP, (priv, pub)) = fromJust $ transmitKey destK dhP
                                              l = length r
                                              kH = KeyHash $ pubKeyToHash pub
                                              epk = encode (kH, (pP, (priv, pub)))
                                              req = Request 0 (sign uK pK $ epk) l r epk t d
                                          pure (priv, pub, Introduce kH pub (sign priv pub $ reqSourceHash kH req) $ IntroContent $ encode req)


addNewPipe :: MonadIO m => PrivKey -> PubKey -> SendFunction -> MVar Pipes -> Request -> m (IO (), [Request] )
addNewPipe uK pK send pV r = if roadPosition r == 0 then case decodeMaybe (encryptedPrivKey r) of
                                                        Nothing -> pure (pure (), [])
                                                        Just (kH, (pP, (prK, pK))) -> let r' = r{encryptedPrivKey = encode (pP :: DHPubKey)} in
                                                                         do onTO <- insertPipe (kH, prK, pK) True r'{neighborSignature = sign uK pK $ reqNeighHash kH r'}
                                                                            pure (onTO, [r'])
                                                 else case decodeMaybe $ encryptedPrivKey r of
                                                        Nothing -> pure (pure (), [])
                                                        Just pP -> do 
                                                                      let (prK, pK) = fromJust $ (exctractKey pP =<< privKeyToDHPrivKey uK)
                                                                          kH = KeyHash $ pubKeyToHash pK
                                                                      insertPipe (kH, prK, pK) False r >>= \onTO -> pure (onTO, [])
    where insertPipe :: MonadIO m => (KeyHash, PrivKey, PubKey) -> Bool -> Request -> m (IO ())
          insertPipe k b r = liftIO $ modifyMVar pV $ insertPipeEntry k b r
          insertPipeEntry (kH, prK, pK) b r pM = do keepLog SourcesLog Important $ "Inserting pipe entry for pipe : " ++ (show . roadToRoadID $ road r) 
                                                                                   ++ "on Road : " ++ (show $ road r)
                                                    let n = if b then (roadPosition r + 1) else (roadPosition r -1)
                                                    specs <- genRoadSpecs (requestTime r) (road r)
                                                    pure (pM{pipesMap = M.insert rID (PipesEntry specs
                                                                           (genWriteFunction b kH prK pK n)
                                                                           (genBreakFunction b kH prK pK n) ) $ pipesMap pM,
                                                             pipesList = rID : pipesList pM}, 
                                                          onTimeOut)
          rID = roadToRoadID (road r)
          genWriteFunction :: Bool -> KeyHash -> PrivKey -> PubKey -> Number -> ComMessage -> IO Bool
          genWriteFunction b kH prK pK n cm = send $ DataPacket kH (sign prK pK $ pipeMessageHash kH b (encode cm)) $ DataContent . encode $ PipeData n b $ encode cm
          genBreakFunction :: Bool -> KeyHash -> PrivKey -> PubKey -> Number -> RawData -> IO Bool
          genBreakFunction b kH prK pK n m = send $ DataPacket kH (sign prK pK $ pipeMessageHash kH b m) $ DataContent . encode $ PipeExit n b m
          onTimeOut = removePipe pV rID


removePipe :: MonadIO m => MVar Pipes -> RoadID -> m ()
removePipe pV rID = liftIO $ modifyMVar_ pV (\p -> pure p{pipesMap =  M.delete rID $ pipesMap p,
                                                         pipesList = delete rID $ pipesList p})



roadToRoadID :: Road -> RoadID
roadToRoadID = RoadID . pubKeyToHash  


newtype ComID = ComID Int deriving (Eq, Ord, Show)
instance Binary ComID where put (ComID i) = put i
                            get = ComID <$> get

data ComMessage = ComInit {comID :: ComID ,
                           comContent :: RawData} |
                  ComData {comID :: ComID ,
                           comContent :: RawData} |
                  ComExit {comID :: ComID ,
                           comContent :: RawData}

instance Show ComMessage where
        show (ComInit cI _) = "COM_INIT = " ++ show cI
        show (ComData cI _) = "COM_DATA = " ++ show cI
        show (ComExit cI _) = "COM_EXIT = " ++ show cI

instance Binary ComMessage where
        put (ComInit cID cnt) = putWord8 0  >> put cID >> put cnt
        put (ComData cID cnt) = putWord8 1 >> put cID >> put cnt
        put (ComExit cID cnt) = putWord8 2 >> put cID >> put cnt
        get = do i <- getWord8
                 case i of
                   0 -> ComInit <$> get <*> get
                   1 -> ComData <$> get <*> get
                   2 -> ComExit <$> get <*> get
                   _ -> fail $ "Unable to parse ComMessage : " ++ show i

