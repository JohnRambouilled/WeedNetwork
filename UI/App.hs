module UI.App where

import UI.Class

import Brick
import Graphics.Vty
import Control.Concurrent.Chan
import Control.Monad


buildApp :: Int -> [String] -> [(String -> String -> IO ()) -> IO ()] -> IO ()
buildApp nbWidgets names hooks = do 
        cfg <- standardIOConfig
        chan <- newChan
        zipWithM_ (registerHook chan) modifiers hooks

        void $ customMain (mkVty cfg) chan newClientApp client

        


 where (client,modifiers) = newClientUI nbWidgets names
       registerHook chan mod hook = hook $ \name string -> writeChan chan $  mod name string
