module UI.App where

import UI.Class

import Brick
import Graphics.Vty
import Control.Concurrent.Chan
import Control.Monad
import qualified Data.Array as A
import Control.Lens
import Data.Ix

import Reactive.Banana.Frameworks


buildApp :: [(String,A.Array (Int,Int) (AddHandler String))] -> IO ()
buildApp binds = do 
        cfg <- standardIOConfig
        chan <- newChan
--        zipWithM_ (registerHook chan) modifiers hooks
--        zipWithM_ (registerModifier chan) modifiers (snd <$> binds)
        sequence $ do (tab,modi) <- zip (snd <$> binds) (modifiers)
                      (i,j) <- range $ A.bounds tab
                      pure $ void $ registerModifier chan modi tab (i,j)
        void $ customMain (mkVty cfg) chan newClientApp client
        
--        pure (void $ customMain (mkVty cfg) chan newClientApp client, buildModifier chan <$> modifiers)
 where (client,modifiers) = newClientUI $ over _2 (snd . A.bounds) <$> binds
       --buildModifier chan mod x y = writeChan chan $ mod x y
       registerModifier chan modi tab (i,j) = register (tab A.! (i,j)) $ \str -> writeChan chan (modi (i,j) str) 

