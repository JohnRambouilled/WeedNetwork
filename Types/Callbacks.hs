module Types.Callbacks where

data Callback e a = Callback {runCallback :: Either e a -> IO () }

call :: Callback e a -> a -> IO ()
call c = runCallback c . Right

closeWithError :: Callback e a -> e -> IO ()
closeWithError c = runCallback c . Left
