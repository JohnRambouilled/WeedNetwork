import Test.Test

import Control.Concurrent
import Control.Monad
  
main :: IO ()
main = do dualTestMain
          forever $ threadDelay (10^6)
