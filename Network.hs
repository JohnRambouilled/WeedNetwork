module Network where

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

compileClient ::[WidgetHandler] -> IO (AddHandler Packet, Handler Packet)
compileClient = (f <$>) . compileClientRes []
    where f (a,b,_) = (a,b)

compileClientRes :: [RessourceID] -> [WidgetHandler] -> IO (AddHandler Packet, Handler Packet, Handler RessourceID)
compileClientRes rIDL wL = do (inE,inH) <- newAddHandler
                              (outE,outH) <- newAddHandler
                              (resE,resH) <- newAddHandler
                              print "building Client"
                              actuate =<< compile (cClient resE outH inE)
                              print "done"
                              pure (outE, inH, resH)
                      
    where cClient :: Frameworks t => AddHandler RessourceID -> Handler Packet -> AddHandler Packet -> Moment t ()
          cClient rh s h = do packetE <- fromAddHandler h
                              resE <- fromAddHandler rh
                              c <- buildClient packetE resE rIDL
                              reactimate $ s <$> clToSend c
                              showClient c wL


