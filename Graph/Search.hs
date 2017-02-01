{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
module Graph.Search where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict     as M
import           Data.Maybe
import           Graph.RoadGraph
import           Graph.Type
import           System.Random
data Searcher = Searcher {_currentID     :: VertexID,
                          _currentVal    :: VertexT,
                          _currentNeighs :: Edges EdgeT,
                          _seen          :: [(VertexID,VertexT)]}

makeLenses ''Searcher

type SearcherT m x = (MonadIO m) => StateT Searcher m x

moveTo :: RoadGraph -> VertexID  -> SearcherT m ()
moveTo g vID
  | isNothing vTM = error $ "Moving to the unregistered vertex: " ++ show vID
  | isJust vTM = do
      prev <- use currentID
      currentID .= vID
      currentVal .= vT
      currentNeighs .= neighs
      seen %= ((vID,vT):)


  where vTM = M.lookup vID $ _vMap g
        (vT,neighs) = fromJust vTM

{-| Idée : retenir pour chaque pipe de chaque sommet la distance qui nous sépare de lui.
    Quand on recherche une route de la source à nous, si on trouve un sommet à partir duquel il existe
    un pipe et que le chemin nous séparant de lui nous parait acceptable, nous l'empruntons et stoppons l'exploration.

    Ajouter un algo de colonies de fourmis ??? [TODO]

    ATTENTION: Les
|-}
randomWalkToMe :: RoadGraph -> Int -> SearcherT m Bool
randomWalkToMe g maxLen = do
          (posID,localPipes) <- (,) <$> use currentID <*> use (currentVal . pipesT . vPipes)
          if posID == me then pure True -- On a trouvé la destination, on s'arrête
            else if maxLen <= 0 then pure False -- si la route est trop longue, on stop l'exploration
            else if not $ null localPipes then do -- s'il y a des pipes passant par ce sommet, je regarde le plus proche de moi
                    let nearest = minimumBy (\pipe1 pipe2 -> _pathLen (snd pipe1) `compare` _pathLen (snd pipe2)) $ M.toList localPipes
                    if _pathLen (snd nearest) > maxLen
                      then moveToRandomNeighbour -- Si le plus proche des pipes est trop loin, je continue la marche aléatoire
                      else walkOnPipe nearest -- Sinon, j'emprunte le pipe
                    pure False
            else moveToRandomNeighbour -- S'il n'y a pas de pipes passant par ici, je poursuis la marche aléatoire.

  where -- Génère un voisin aléatoire que l'on n'a pas visité
        randomNeighbour = do
          seen <- map fst <$> use seen
          neighs <- M.filterWithKey (\vID _ -> not $ vID `elem` seen) . _eMap <$> use currentNeighs -- On considère un voisin que l'on n'a pas déjà visité
          r <- liftIO $ randomRIO (0,M.size neighs-1)
          pure $ M.elemAt r neighs
        moveToRandomNeighbour = do
          (nxtID,_) <- randomNeighbour
          moveTo g nxtID
          randomWalkToMe g (maxLen - 1)
        -- Progresse le long d'un pipe jusqu'à moi
        walkOnPipe (pipeID,(PipeNode prevM nextM _ dir _)) = do
          posID <- use currentID
          if posID == me then pure True
            else case dir of
              NextD -> do moveTo g (fromJust nextM)
                          localPipes <- use (currentVal . pipesT . vPipes)
                          walkOnPipe (pipeID, fromJust $ M.lookup pipeID localPipes)
              PrevD -> do moveTo g (fromJust prevM)
                          localPipes <- use (currentVal . pipesT . vPipes)
                          walkOnPipe (pipeID, fromJust $ M.lookup pipeID localPipes)




