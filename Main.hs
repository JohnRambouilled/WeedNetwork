{-# LANGUAGE FlexibleContexts #-}
module Main where
import Data.Binary
import Data.ByteString.Lazy(empty, length)

import Crypto
import Packet
import Ressource
import Client
import Sources
import Protocol
import Gateway
import Class
import Proxy
import Timer
import Log
import Test hiding (runChildren, introduceThread)


import Sniffer.Ethernet
import Network.Socket
import Control.Monad.State
import Control.Concurrent
import Communication

tcp_socketName = "testounet_tcp.socket"



--main = runWeeds leechMain
--main = runWeeds seedMain

main = do testCL <- testBinaire leechMain seedMain -- testChaine [leechMain, relayMain, seedMain]
          print "running test"
          let dumpClients = concat <$> (forM testCL $ (fst <$>) . runStateT dumpClient . client)
          repeatEach (ctimer . client $ head testCL) (putStrLn =<< dumpClients) 2 >> pure ()
          mVL <- forM testCL $ runChildren . run
          forM_ mVL readMVar
   

--main = fullGraphMain 5
                    

runWeeds :: ClientBehaviour -> IO ()
runWeeds c = do (pubkey,privkey) <- generateKeyPair 1024
                let me = SourceID $ KeyHash $ pubKeyToHash pubkey
                sock <- newRawSocket
                -- Initialize the client
                client <- genClient me privkey pubkey $ \d -> do putStrLn $ "sending packet : " ++ (show . Data.ByteString.Lazy.length $ encode d)
                                                                 writePacket sock d
                -- Initialize the ctimer - checks every 10 seconds [TODO]
                tim <- runChildren $ startTimer (ctimer client) 1
                -- Launching the specifieds actions
                srvc <- runChildren $ c client $ writePacket sock
                repeatEach (ctimer client) (putStrLn =<< fst <$> runStateT dumpClient client) 2 >> pure ()
                -- Starts the networks
                weed <- runChildren $ loop $ listenPacket client sock
                -- Waits for childrens
                forM_ [weed,tim,srvc] $ readMVar
    where loop f = f >> loop f
          listenPacket client sock = do
                                        b <- waitForPkt sock
                                        case b of
                                                Nothing -> keepLog ClientLog Normal "received unreadable packet"
                                                Just pkt -> do keepLog ClientLog Normal "received packet!"
                                                               fst <$> runStateT (onPacket pkt) client >>= mapM_ (writePacket sock)



runChildren :: IO () -> IO (MVar ())
runChildren x = do r <- newEmptyMVar
                   forkFinally x (\err -> print err >> putMVar r ())
                   return r


