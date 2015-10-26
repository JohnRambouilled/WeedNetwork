module Main where
import UI.ShowClient
import Network
--import Test

main = ciEvents <$>compileClient >>= renderClient --testMainRes
