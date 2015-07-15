{-# LANGUAGE FlexibleContexts #-}
module Log where
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Time

type Log = [LogMsg]

data LogMsg = LogMsg {logStatus :: LogStatus,
                      logModule :: ModuleName,
                      logMsg :: String}
instance Show LogMsg where show (LogMsg sts name msg) = show sts ++ "  " ++ show name ++ " | " ++ show msg


data LogStatus = Normal | Important | Error | Suspect deriving Show

data ModuleName = CryptoLog | NeighborLog | RessourcesLog | RoutingLog |  SourcesLog | CommunicationLog | ProtocolLog | TimerLog 
                | ProxyLog | GatewayLog | ClientLog | TestLog | TransportLog
  deriving Show

keepLog :: MonadWriter Log m => ModuleName -> LogStatus -> String -> m ()
keepLog m st msg = case m of
--                        CryptoLog -> printLog
--                        CommunicationLog -> printLog
--                        ProtocolLog -> printLog
                        ProxyLog -> printLog
                        GatewayLog -> printLog
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

