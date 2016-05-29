module Types.Callbacks where
import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map as M

import Data.Time.Clock.POSIX


import Packets
import Types.Crypto
--type STMIO = WriterT [IO ()] STM

data Log = RawLog String


data STMReader = STMReader { stmTime :: Time, 
                             stmLog :: Log -> STMIO (), 
                             stmSender :: RawPacket -> STMIO (),
                             stmKeys :: KeyPair,
                             stmDHKeys :: DHKeyPair,
                             stmMe :: UserID} 
type STMIO = WriterT [IO ()] (ReaderT STMReader STM)


data Callback e a = Callback {runCallback :: Either e a -> STMIO () }

call :: Callback e a -> a -> STMIO ()
call c = runCallback c . Right

liftIOLater :: IO () -> STMIO ()
liftIOLater io = tell [io]

closeWithError :: Callback e a -> e -> STMIO ()
closeWithError c = runCallback c . Left


getTime :: STMIO Time
getTime = stmTime <$> lift ask

logM :: Log -> STMIO ()
logM l = join $ ($l) <$> lift (asks stmLog)

whoAmI :: STMIO UserID
whoAmI = stmMe <$> lift ask


liftSTM :: STM a -> STMIO a
liftSTM = lift . lift


stmRead :: TVar m -> STMIO m
stmRead = liftSTM . readTVar


stmWrite :: TVar m -> m ->  STMIO ()
stmWrite m = liftSTM . writeTVar m

stmModify :: TVar m -> (m -> m) -> STMIO ()
stmModify m = liftSTM . modifyTVar m 


runSTMIO :: STMReader -> STMIO a -> IO a
runSTMIO reader stmIOAct = do (a,w) <- atomically stmAct
                              sequence_ w
                              pure a
 where readerAct = runWriterT stmIOAct
       stmAct = runReaderT readerAct reader
                      
deleteLookup :: (Ord k) => k -> M.Map k v -> (Maybe v, M.Map k v)
deleteLookup clef map = M.updateLookupWithKey (\_ _ -> Nothing) clef map
