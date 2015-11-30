module Test where


import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Binary
import Control.Monad.Writer

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
              print "Building Clients"
              ciL <- forM bananWriterL compileClient
              print "Building display"
              mv <- launchApp . concat $ fst . snd <$> ciL
              mapM_ genTest (zip tcL $ snd . snd <$> ciL)
              print "Launching interface"
              let ciLZ = zip (fst <$> ciL) $ tcListen <$> tcL
              print "registering communications"
              forM_ ciLZ $ \(ci, nl) -> forM_ nl $ \i -> ciOutput ci `register` ciInput (fst $ ciL !! i)
              readMVar mv

    where bananWriterL = map buildTestClient tcL :: [BananWriter (ShowClient, (Handler RessourceID, Handler RessourceID))]
          buildTestClient :: TestClient -> BananWriter (ShowClient, (Handler RessourceID, Handler RessourceID))
          buildTestClient tc = do offH <- extractHandler $ \c rID -> offerRessource (clRessources c) (testValidity, encode $ show rID, rID)
                                  resH <- extractHandler clResearch
                                  sc <- renderClient showModuleList
                                  pure (sc, (offH, resH))
          genTest :: (TestClient, (Handler RessourceID, Handler RessourceID)) -> IO () 
          genTest (tc, (offH, resH)) = do forM_ (tcOfferedR tc) offH
                                          forM_ (tcResearch tc) resH
              

              
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

