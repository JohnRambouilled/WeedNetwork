{- TODO Continuations... -}
module Routes.Table where

import qualified Data.Map as M
import Control.Monad.Cont -- TODO
import Data.Tree 
import Data.Tree.Zipper
import Data.Maybe


{- Regarde le suivant de la forêt, si ce n'est pas le bon, recommence -}
exploreForest1' :: (Eq a) => (TreePos Full a -> Maybe (TreePos Full a)) -> a -> TreePos Full a -> Maybe (TreePos Full a)
exploreForest1' fun v t = do c <- fun t
                             if label c == v then pure c
                                    else exploreForest1' next v c
{- Cherche parmis les fils d'un arbre -}
exploreTree1 :: (Eq a) => a -> TreePos Full a -> Maybe (TreePos Full a)
exploreTree1 a t = if label t == a then pure t else exploreForest1' firstChild a t

{- Cherche un chemin dans un arbre -}
exploreTreePath :: (Eq a) => [a] -> TreePos Full a -> Maybe (TreePos Full a)
exploreTreePath [] t = pure t
exploreTreePath (x:xs) t = exploreTree1 x t >>= exploreTreePath xs

{-| Construit un arbre à partir d'une route |-}
makeTree :: [a] -> Tree a
makeTree [] = error "makeTree on empty list"
makeTree [x] = Node x [] 
makeTree (x:xs) = Node x [makeTree xs]

{-| Insère une road dans un arbre |-}
insertRoad' :: (Eq a) => [a] -> TreePos Full a -> TreePos Full a
insertRoad' [] t = t
insertRoad' road@(x:xs) t = case exploreTree1 x t of
                                Nothing -> insert (makeTree road) (children t)
                                Just pos -> insertRoad' xs pos
insertRoadP :: (Eq a) => [a] -> TreePos Full a -> TreePos Full a
insertRoadP road t = root $ insertRoad' road t

insertRoad :: (Eq a) => [a] -> Tree a -> TreePos Full a
insertRoad road = insertRoadP road . fromTree

{-| Retire une route et toutes ses eventuelles prolongations |-}
removeSubRoadP :: (Eq a) => [a] -> TreePos Full a -> Maybe (Tree a, Tree a)
removeSubRoadP r t = do retPos <- exploreTreePath r t
                        cleanedTree <- root <$> parent (delete retPos) 
                        pure (toTree cleanedTree, toTree retPos)
--removeSubRoadP r t = root <$> (join $ parent . delete <$> exploreTreePath r t)

removeSubRoad :: (Eq a) => [a] -> Tree a -> Maybe (Tree a, Tree a) --(TreePos Full a)
removeSubRoad road = removeSubRoadP road . fromTree

