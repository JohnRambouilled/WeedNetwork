module UI.App where

import UI.Class

import Brick
import Graphics.Vty
import Control.Concurrent.Chan
import Control.Monad


buildApp :: Int -> [String] -> IO (IO (), [String -> String -> IO ()])
buildApp nbWidgets names = do 
        cfg <- standardIOConfig
        chan <- newChan
--        zipWithM_ (registerHook chan) modifiers hooks

        pure (void $ customMain (mkVty cfg) chan newClientApp client, buildModifier chan <$> modifiers)
 where (client,modifiers) = newClientUI nbWidgets names
       buildModifier chan mod x y = writeChan chan $ mod x y
