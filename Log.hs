module Log where
import Control.Monad.State
import Data.List
import Data.Time

data LogStatus = Normal | Important | Error | Suspect deriving Show

data ModuleName = CryptoLog | NeighborLog | RessourcesLog | RoutingLog |  SourcesLog | CommunicationLog | ProtocolLog | TimerLog | ProxyLog | GatewayLog | ClientLog | TestLog 
  deriving Show

keepLog :: MonadIO m => ModuleName -> LogStatus -> String -> m ()
keepLog m st msg = case m of
--                        CryptoLog -> printLog
--                        CommunicationLog -> printLog
--                        ProtocolLog -> printLog
                        ProxyLog -> printLog
                        GatewayLog -> printLog
--                        ClientLog -> printLog
--                        TestLog -> printLog
                        _ -> pure ()
    where   printLog :: MonadIO m => m ()
            printLog = liftIO $ do t <- getCurrentTime
                                   putStrLn $ show t ++ " ## " ++ show st ++ " : " ++ show m 
                                   putStrLn $ "           --> " ++ msg



