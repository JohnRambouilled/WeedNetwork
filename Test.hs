module Test where


import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Binary

import UI.ShowClient
import UI.App
import Client
import Crypto
import Neighbors
import Timer
import Ressource
import Network


mainTestRelay :: IO ()
mainTestRelay = testMain [TestClient [res1] [] True [1],
                          TestClient [] [] True [0,2],
                          TestClient [] [res1] True [1]]
        where res1 = RessourceID (encode "This is the shit")



mainTestBinaire :: IO ()
mainTestBinaire = testMain [TestClient [res1] [] True [1],
                            TestClient [] [res1] True [0]]
        where res1 = RessourceID (encode "This is the shit")


data TestClient = TestClient {tcOfferedR :: [RessourceID],
                              tcResearch :: [RessourceID],
                              tcShow :: Bool,
                              tcListen :: [Int]}


testValidity = 5 :: Time


testMain :: [TestClient] -> IO ()
testMain tcL = do 
              print "Building display"
              ciL <- forM tcL (pure compileClient)
              print "Launching interface"
              mv <- myForkIO $ renderClients ciL
              let ciLZ = zip ciL tcL
              print "registering communications"
              forM_ ciLZ $ \(ci,tc) -> do forM_ (tcOfferedR tc) $ \rID -> ciOfferRessource ci (testValidity, encode $ show rID, rID)
                                          forM_ (tcListen tc) $ \i -> register (ciOutput ci) (ciInput $ ciL !! i)
                                          forM_ (tcResearch tc) $ ciResearch ci
              readMVar mv

    where myForkIO :: IO () -> IO (MVar ())
          myForkIO io = do mvar <- newEmptyMVar
                           forkIO (io >> putMVar mvar ())
                           return mvar
              

              
{-
leakTestMain :: IO ()
leakTestMain = do 
              dummyKeys <- generateKeyPair
              (displayIO, handles) <- buildApp (length moduleNameList) moduleNameList
              (outC,inC) <- compileClient handles
              forkIO .forM_ neighList $ \i -> do waitFor 0.1   
                                                 inC . Left $ sendNeighIntro (KeyHash i) dummyKeys i
              displayIO
    where neighList :: [RawData]
          neighList = encode <$> ([1..] :: [Int])




leakTestMainNoUI :: IO ()
leakTestMainNoUI = do 
              dummyKeys <- generateKeyPair
--              (displayIO, handles) <- buildApp (length moduleNameList) moduleNameList
              (outC,inC) <- compileClient [] --handles
              forM_ neighList $ \i -> do waitFor 0.1   
                                         inC . Left $ sendNeighIntro (KeyHash i) dummyKeys i
        --      displayIO
    where neighList :: [RawData]
          neighList = encode <$> ([1..] :: [Int])



testMainRes :: IO ()
testMainRes = do 
              print "Building display"
              (displayIO, handles) <- buildApp (2 * moduleShowCount) (moduleNameList ++ moduleNameList)
              let (c1H,c2H) = splitAt moduleShowCount handles
              print "Compiling clients"
              ((out1,in1,res1),(out2,in2,res2)) <- (,) <$> compileClientRes [resID1] c1H <*> compileClientRes [] c2H
              newRepeater (Just 10) 2 $ res2 resID1
              print "registering communications"
              print "Launching display"
              displayIO
    where resID1 = RessourceID $ encode "Je suis 1!!"
-}

