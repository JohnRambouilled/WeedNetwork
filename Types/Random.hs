module Types.Random where
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           System.Random

class (Monad m) => MonadRandom m where
  putStdGen :: StdGen -> m ()
  asksStdGen :: m StdGen


getRandomInt :: (MonadRandom m, Random b, Monad m) => (b, b) -> m b
getRandomInt r = do
  gen <- asksStdGen
  let (ret, gen') = randomR r gen
  putStdGen gen' >> pure ret

instance (MonadRandom m) => MonadRandom (ReaderT a m) where
  putStdGen = lift . putStdGen
  asksStdGen = lift asksStdGen

instance (MonadRandom m, Monoid a) => MonadRandom (WriterT a m) where
  putStdGen = lift . putStdGen
  asksStdGen = lift asksStdGen

instance (MonadRandom m) => MonadRandom (StateT a m) where
  putStdGen = lift . putStdGen
  asksStdGen = lift asksStdGen

