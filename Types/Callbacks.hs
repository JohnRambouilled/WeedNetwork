module Types.Callbacks where
import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.Writer
import qualified Data.Map as M

type STMIO = WriterT [IO ()] STM


data Callback e a = Callback {runCallback :: Either e a -> STMIO () }

call :: Callback e a -> a -> STMIO ()
call c = runCallback c . Right

liftIOLater :: IO () -> STMIO ()
liftIOLater io = tell [io]

closeWithError :: Callback e a -> e -> STMIO ()
closeWithError c = runCallback c . Left


deleteLookup :: Ord k => k -> TVar (M.Map k a) -> STMIO (Maybe a)
deleteLookup k mv = withTVar mv $ pure . M.updateLookupWithKey (\ _ _ -> Nothing) k 


withTVar :: TVar a -> (a -> STMIO (b,a))  -> STMIO b
withTVar v f = do m <- lift $ readTVar v
                  (b,a) <- f m
                  lift $ writeTVar v a
                  pure b
