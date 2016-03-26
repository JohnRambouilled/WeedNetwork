{-# LANGUAGE ImpredicativeTypes, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Network where

import Reactive.Banana
import Reactive.Banana.Frameworks hiding (liftIO)
import qualified Data.Map as M
import Control.Monad
import Control.Concurrent
import Data.Binary
import Control.Monad.Writer

--import UI.ShowClient
--import UI.App
import Class
import Client
import Ed25519
import Crypto
import Neighbors
import Timer
import Ressource
import Routing
import PipePackets
import Pipes


type ClientEvent a = Client -> Handler a -> MomentIO ()
type ClientHandler a = Client -> Event a -> MomentIO ()
type BananAction = Client -> MomentIO ()
type BananWriter = WriterT [BananAction] IO
type ShowHandle = AddHandler String
type Logs = String

data ClientInterface = ClientInterface{ ciInput :: Handler Packet,
                                        ciOutput :: AddHandler Packet,
                                        ciIDs :: (UserID, KeyPair, DHKeyPair)}

compileClient :: BananWriter a -> IO (ClientInterface, a)
compileClient bwA = do (inE,inH) <- newAddHandler
                       (outE, outH) <- newAddHandler
                       (keys,dhKeys) <- (,) <$> generateKeyPair <*> generateDHKeyPair

                       (a, act) <- runWriterT bwA

                       let uID = computeHashFromKey $ fst keys
                           mkClient :: MomentIO ()
                           mkClient = do inEv <- fromAddHandler inE 
                                         c <- buildClient inEv dhKeys keys uID
                                         reactimate $ outH <$> clToSend c
                                         forM_ act ($c)
                                         pure ()
                           ci = ClientInterface inH outE (uID, keys, dhKeys)

                       actuate =<< compile mkClient 
                       pure (ci,a)


getClientEvent :: ClientEvent a -> BananWriter (AddHandler a)
getClientEvent ce = do (e,h) <- liftIO $ newAddHandler
                       tell [flip ce $ h]
                       pure e
getClientHandler :: ClientHandler a -> BananWriter (Handler a)
getClientHandler ch = do (e,h) <- liftIO $ newAddHandler
                         tell [\c -> ch c =<< fromAddHandler e]
                         pure h


extractAddHandler :: (Client -> Event a) -> BananWriter (AddHandler a)
extractAddHandler f = getClientEvent $ \c h -> reactimate (h <$> f c)

extractHandler :: (Client -> Handler a) -> BananWriter (Handler a)
extractHandler f = getClientHandler ch
    where ch c e = reactimate $ f c <$> e

extractKeyList :: (Client -> BehaviorC (M.Map k a)) -> BananWriter (AddHandler [k])
extractKeyList f = extractAddHandler $ fmap M.keys . bcChanges . f


