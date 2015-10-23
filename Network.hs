module Network where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M
import Control.Monad
import UI.ShowClient
import UI.App
import Client


testMain :: IO ()
testMain = do print "Building display"
              (displayIO, handles) <- buildApp (2 * length moduleNameList) (moduleNameList ++ moduleNameList)
              let (c1H,c2H) = splitAt (length moduleNameList) handles
              print "Compiling clients"
              ((out1,in1),(out2,in2)) <- (,) <$> compileClient c1H <*> compileClient c2H
              print "registering communications"
              register out1 in2
              register out2 in1
              print "Launching display"
              displayIO

compileClient :: [WidgetHandler] -> IO (AddHandler Packet, Handler Packet)
compileClient wL = do (inE,inH) <- newAddHandler
                      (outE,outH) <- newAddHandler
                      print "building Client"
                      actuate =<< compile (cClient outH inE)
                      print "done"
                      pure (outE, inH)
                      
    where cClient :: Frameworks t => Handler Packet -> AddHandler Packet -> Moment t ()
          cClient s h = do packetE <- fromAddHandler h
                           c <- buildClient packetE
                           reactimate $ s <$> clToSend c
                           showClient c wL


