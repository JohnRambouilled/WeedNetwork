module Test where


import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M
import Control.Monad
import Control.Concurrent
import Data.Binary

import UI.ShowClient
import UI.App
import Client
import Crypto
import Neighbors
import Timer
import Ressource
import Network

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
              register out1 in2
              register out2 in1
              print "Launching display"
              displayIO
    where resID1 = RessourceID $ encode "Je suis 1!!"


testMain :: IO ()
testMain = do print "Building display"
              (displayIO, handles) <- buildApp (2 * moduleShowCount) (moduleNameList ++ moduleNameList)
              let (c1H,c2H) = splitAt moduleShowCount handles
              print "Compiling clients"
              ((out1,in1),(out2,in2)) <- (,) <$> compileClient c1H <*> compileClient c2H
              print "registering communications"
              register out1 in2
              register out2 in1
              print "Launching display"
              displayIO


