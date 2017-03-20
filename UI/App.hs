module UI.App where

import UI.Class

import Brick
import Brick.BChan
import Graphics.Vty
import Control.Concurrent
import Control.Monad
import qualified Data.Array as A
import Control.Lens
import Data.Ix

maxChanSize = 100 :: Int
updateDelay = 10^5 :: Int -- In microseconds

newtype Displayer = Displayer { runDisplayer :: IO String }

type ShowClient = [(String, A.Array (Int,Int) Displayer )]

buildApp :: ShowClient -> IO ()
buildApp binds = do 
        print "buildApp : 1 "
        cfg <- standardIOConfig
        print "buildApp : 2 "
        chan <- newBChan maxChanSize
        print "buildApp : 3 "
--        zipWithM_ (registerHook chan) modifiers hooks
--        zipWithM_ (registerModifier chan) modifiers (snd <$> binds)
        forkIO $ updateThread chan
        print "buildApp : 4 "
        print "buildApp : 5 "
        void $ customMain (mkVty cfg) (Just chan) newClientApp client
        
--        pure (void $ customMain (mkVty cfg) chan newClientApp client, buildModifier chan <$> modifiers)
 where (client,modifiers) = newClientUI $ over _2 (snd . A.bounds) <$> binds
       --buildModifier chan mod x y = writeChan chan $ mod x y
       --registerModifier chan modi tab (i,j) = register (tab A.! (i,j)) $ \str -> writeBChan chan (modi (i,j) str) 
       updateWidget chan modi tab (i,j) = do str <- runDisplayer $ tab A.! (i,j)
                                             writeBChan chan $ modi (i,j) str
       updateApp chan = sequence_ $ do (tab,modi) <- zip (snd <$> binds) (modifiers)
                                       (i,j) <- range $ A.bounds tab
                                       pure . void $ updateWidget chan modi tab (i,j)
       updateThread chan = forever $ updateApp chan >> threadDelay updateDelay

{-
buildApp :: ShowClient -> IO ()
buildApp binds = do 
        print "buildApp : 1 "
        cfg <- standardIOConfig
        print "buildApp : 2 "
        chan <- newBChan maxChanSize
        print "buildApp : 3 "
--        zipWithM_ (registerHook chan) modifiers hooks
--        zipWithM_ (registerModifier chan) modifiers (snd <$> binds)
        print "buildApp : 4 "
        sequence_ $ do (tab,modi) <- zip (snd <$> binds) (modifiers)
                       (i,j) <- range $ A.bounds tab
                       pure $ void $ registerModifier chan modi tab (i,j)
        print "buildApp : 5 "
        void $ customMain (mkVty cfg) (Just chan) newClientApp client
        
--        pure (void $ customMain (mkVty cfg) chan newClientApp client, buildModifier chan <$> modifiers)
 where (client,modifiers) = newClientUI $ over _2 (snd . A.bounds) <$> binds
       --buildModifier chan mod x y = writeChan chan $ mod x y
       registerModifier chan modi tab (i,j) = register (tab A.! (i,j)) $ \str -> writeBChan chan (modi (i,j) str) 
-}
