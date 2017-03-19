import Test.Test

import Control.Concurrent
import Control.Monad
  
main :: IO ()
main = do tg <- dualTestGraph
          forever $ threadDelay (10^6)
