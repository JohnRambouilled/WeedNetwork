module Main where
import UI.ShowClient
import Network
--import Test

main = pure <$> ciEvents <$>compileClient >>= renderClients --testMainRes
