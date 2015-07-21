{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Log where
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS.Lazy
import Data.List
import Data.Time

type Log = [LogMsg]

type LogFunction = Log -> IO ()

printLog :: Log -> IO ()
--printLog logs = forM logs putStrLn . ("\t" ++) . show
printLog [] = pure ()
printLog ll = putStrLn . unlines . map (("\t"++).show) $ ll 

class (MonadWriter Log m, MonadIO m) => LogIO m
--class alias LogIO m = (MonadWriter Log m, MonadIO m)
--instance (MonadIO m) => LogIO (RWST p Log s m)
type IOLog = WriterT Log IO

instance (MonadWriter Log m, MonadIO m) => LogIO m

liftLog :: LogIO m => IOLog a -> m a
liftLog a = do (r, l) <- liftIO $ runWriterT a
               tell l >> pure r

data LogMsg = LogMsg {logStatus :: LogStatus,
                      logModule :: ModuleName,
                      logMsg :: String}
instance Show LogMsg where show (LogMsg sts name msg) = show sts ++ "  " ++ show name ++ " | " ++ show msg


data LogStatus = Normal | Important | Error | Suspect deriving Show

data ModuleName = CryptoLog | NeighborLog | RessourcesLog | RoutingLog |  SourcesLog | CommunicationLog | ProtocolLog | TimerLog 
                | ProxyLog | GatewayLog | ClientLog | TestLog | TransportLog
  deriving Show

keepLog :: LogIO m => ModuleName -> LogStatus -> String -> m ()
keepLog m st msg = case m of
--                        CryptoLog -> printLog
--                        CommunicationLog -> printLog
--                        ProtocolLog -> printLog
--                        ProxyLog -> printLog
--                        GatewayLog -> printLog
--                        ClientLog -> printLog
--                        TestLog -> printLog
                        TransportLog -> printLog
                        _ -> case st of
                               Normal -> pure ()
                               _ -> pure () --
      where printLog = tell [LogMsg st m msg]
{-    where   printLog :: MonadIO m => m ()
            printLog = liftIO $ do t <- getCurrentTime
                                   putStrLn ""
                                   putStrLn $ show t 
                                   putStrLn $ show st ++ " : " ++ show m ++ "    -->   " ++ msg

-}

