module Module where
import Data.ByteString
import Reactive.Banana.Combinators
import Data.Binary
import Control.Event.Handler
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M





type MapModule t k b = Behavior t (IO (M.Map k b))

--type MapModuleOrders k b = MapModuleAdd k b |
--                           MapModuleDelete k b


type MapModuleEvents t k b = Event t (StateT (M.Map k b) IO ())



accumMapModule :: MapModuleEvents t k b -> MapModule t k b
accumMapModule mme = accumB (pure M.empty) (runS <$> mme)
    where runS :: StateT a IO () -> IO a -> IO a
          runS f = (snd <$>) . join . (runStateT f <$>)
          









