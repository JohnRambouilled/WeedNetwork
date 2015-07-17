{-# LANGUAGE FlexibleContexts #-}
module Main where
import Data.Binary
import Data.ByteString.Lazy(empty, length)
import Control.Monad.RWS.Lazy

import Client.Crypto
import Client.Packet
import Client.Ressource
import Client
import Client.Sources
import Client.Protocol
import Gateway
import Client.Class
import Proxy
import Timer
import Log
import Test hiding (runChildren, introduceThread)


import Sniffer.Ethernet
import Network.Socket
import Control.Monad.State
import Control.Concurrent
import Client.Communication
import Crypto.Random

tcp_socketName = "testounet_tcp.socket"



--main = runWeeds leechMain
--main = runWeeds seedMain

main = do testCL <- testBinaire (leechMain leechLog, leechLog) (seedMain seedLog, seedLog) -- testChaine [leechMain, relayMain, seedMain]
          print "running test"
          let dumpClients = concat <$> (forM testCL $ (fst <$>) . runStateT dumpClient . client)
          repeatEach (ctimer . client $ head testCL) (liftIO . putStrLn =<< dumpClients) 10 >> pure ()
          mVL <- forM testCL $ runChildren . run
          forM_ mVL readMVar
   where leechLog ll = do putStrLn "# LEECH : " 
                          forM_ ll $ \x -> putStrLn (show x ++ "\n          ")
         seedLog ll  = do putStrLn "# SEED  : " 
                          forM_ ll $ \x -> putStrLn (show x ++ "\n          ")
                          pure ()
--main = fullGraphMain 5
                    
{-
runWeeds :: ClientBehaviour -> IO ()
runWeeds c = do gV <- newMVar =<< drgNew--getSystemDRG
                (pubkey,privkey) <- genKeyPairMVar gV
                let me = SourceID $ KeyHash $ pubKeyToHash pubkey
                sock <- newRawSocket
                -- Initialize the client
                client <- genClient gV me privkey pubkey $ \d -> do putStrLn $ "sending packet : " ++ (show . Data.ByteString.Lazy.length $ encode d)
                                                                    writePacket sock d
                -- Initialize the ctimer - checks every 10 seconds [TODO]
                tim <- runChildren $ startTimer (ctimer client) 1
                -- Launching the specifieds actions
                srvc <- runChildren $ c client $ writePacket sock
                repeatEach (ctimer client) (liftIO . putStrLn =<< fst <$> runStateT dumpClient client) 10 >> pure ()
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
                                                               --fst <$> runStateT (onPacket pkt) client >>= mapM_ (writePacket sock)
                                                               (ret,_,logs) <- runRWST onPacket pkt client
							       forM logs print
							       mapM_ (writePacket sock) (ret :: [Packet])

                                                               

-}

runChildren :: IO () -> IO (MVar ())
runChildren x = do r <- newEmptyMVar
                   forkFinally x (\err -> print err >> putMVar r ())
                   return r


