module Main where

import Test.Test

import Control.Concurrent
import Control.Monad
  
main :: IO ()
main = do
          --dualTestMain
          tripleTest
          forever $ threadDelay (10^6)
