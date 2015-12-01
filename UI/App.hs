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

type ShowClient = [(String, A.Array (Int,Int) (AddHandler String))]

buildApp :: ShowClient -> IO ()
buildApp binds = do 
        print "buildApp : 1 "
        cfg <- standardIOConfig
        print "buildApp : 2 "
        chan <- newChan
        print "buildApp : 3 "
--        zipWithM_ (registerHook chan) modifiers hooks
--        zipWithM_ (registerModifier chan) modifiers (snd <$> binds)
        print "buildApp : 4 "
        sequence_ $ do (tab,modi) <- zip (snd <$> binds) (modifiers)
                       (i,j) <- range $ A.bounds tab
                       pure $ void $ registerModifier chan modi tab (i,j)
        print "buildApp : 5 "
        void $ customMain (mkVty cfg) chan newClientApp client
        
--        pure (void $ customMain (mkVty cfg) chan newClientApp client, buildModifier chan <$> modifiers)
 where (client,modifiers) = newClientUI $ over _2 (snd . A.bounds) <$> binds
       --buildModifier chan mod x y = writeChan chan $ mod x y
       registerModifier chan modi tab (i,j) = register (tab A.! (i,j)) $ \str -> writeChan chan (modi (i,j) str) 

