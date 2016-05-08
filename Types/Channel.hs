module Types.Channel where

import qualified Data.Map as M
import Control.Monad
import Control.Lens
import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.List as L

newtype Channel e = Channel{runChannel :: TChan (Either () e)}
newtype BrdChannel e = BrdChannel{runBrdChannel :: TChan (Either () e)}


-- | Duplique un channel de broadcast (attention, le channel créé DOIT être vidé).  (sous peine de castration) ... (violente)
listenBrdChannel :: BrdChannel e -> IO (Channel e)
listenBrdChannel b = Channel <$> (atomically . dupTChan $ runBrdChannel b)


newChannel :: STM (Channel e)
newChannel = Channel <$> newTChan

fireChannel :: Channel e -> e -> IO ()
fireChannel c = atomically . writeTChan (runChannel c) . Right

closeChannel :: Channel e -> IO ()
closeChannel c = atomically . writeTChan (runChannel c) $ Left ()

-- | fmap sur chaque occurrence, avec IO (crée un nouveau TChan)
register :: Channel e -> (e -> IO a) -> IO (Channel a)
register c f = do c' <- Channel <$> newTChanIO
                  forkIO $ loop c'
                  pure c'
    where loop c' = do e <- atomically $ readTChan (runChannel c)
                       case e of Left () -> closeChannel c'
                                 Right a -> fireChannel c' =<< f a

-- | fmap sur chaque occurrence et discard le résultat (ne crée pas de TChan)
register_ :: Channel e -> (e -> IO ()) -> IO ()
register_ c f = do forkIO $ loop
                   pure ()
     where loop = do r <- atomically $ readTChan (runChannel c)
                     case r of Left () -> pure ()
                               Right e -> f e >> loop


registerClose :: Channel e -> IO () -> IO ()
registerClose c a = void $ forkIO loop
        where loop = do e <- atomically $ readTChan (runChannel c)
                        case e of Left () -> a
                                  Right _ -> loop







