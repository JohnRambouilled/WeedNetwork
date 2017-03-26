{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE UnicodeSyntax    #-}
module Types.Graph.Search where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict       as M
import           Data.Maybe
import           Debug.Trace
import           System.Random
import           Types.Graph.Export
import           Types.Graph.RoadGraph
import           Types.Graph.Type
import           Types.Random
data Searcher = Searcher {_me            ∷ VertexID,
                          _currentID     :: VertexID,
                          _currentVal    :: VertexT,
                          _currentNeighs :: Edges EdgeT,
                          _seen          :: [(VertexID,VertexT)]}

makeLenses ''Searcher

type SearcherT m x = (MonadRandom m) => StateT Searcher m x
searchRoad
  :: MonadRandom m =>
     VertexID  -- Me
     -> VertexID -- TargetID
     -> Int -- Depth
     -> Graph VertexT EdgeT
     -> m (Maybe [(VertexID, VertexT)])
searchRoad me targetID depth g = case (M.lookup targetID (_vMap g)) of
  Nothing               -> pure Nothing
  Just (targetT,neighs) -> searchRoad' me targetID targetT neighs depth g
searchRoad' ∷ (MonadRandom m) ⇒ VertexID → VertexID → VertexT → Edges EdgeT → Int → RoadGraph → m (Maybe [(VertexID,VertexT)])
searchRoad' me target targetT neighs depth g = do
  r ← runStateT (randomWalkToMe g depth) $ Searcher me target targetT neighs [(target,targetT)]
  if fst r then pure $ Just $ _seen $ snd r else pure Nothing

moveTo :: RoadGraph -> VertexID  -> SearcherT m ()
moveTo g vID
  | isNothing vTM = error $ "Moving to the unregistered vertex: " ++ show vID
  | isJust vTM = do
      prev <- use currentID
      currentID .= trace (showVID vID)vID
      currentVal .= vT
      currentNeighs .= neighs
      seen %= ((vID,vT):)


  where vTM = M.lookup vID $ _vMap g
        (vT,neighs) = fromJust vTM

{-| Idée : retenir pour chaque pipe de chaque sommet la distance qui nous sépare de lui.
    Quand on recherche une route de la source à nous, si on trouve un sommet à partir duquel il existe
    un pipe et que le chemin nous séparant de lui nous parait acceptable, nous l'empruntons et stoppons l'exploration.

    Ajouter un algo de colonies de fourmis ??? [TODO]


|-}
randomWalkToMe :: RoadGraph -> Int -> SearcherT m Bool
randomWalkToMe g maxLen = do
          (me,posID,localPipes) ← (,,) <$> use me <*> use currentID <*> use (currentVal . pipesT . vPipes)
          if posID == me then pure True -- On a trouvé la destination, on s'arrête
            else if maxLen <= 0 then pure False -- si la route est trop longue, on stop l'exploration
            else if not $  null localPipes then do -- s'il y a des pipes passant par ce sommet, je regarde le plus proche de moi
                    let nearest = minimumBy (\pipe1 pipe2 -> _pathLen (snd pipe1) `compare` _pathLen (snd pipe2)) $ M.toList localPipes
                    if _pathLen (snd nearest) > maxLen
                      then moveToRandomNeighbour -- Si le plus proche des pipes est trop loin, je continue la marche aléatoire
                      else do walkOnPipe nearest -- Sinon, j'emprunte le pipe
                              pure True
            else moveToRandomNeighbour -- S'il n'y a pas de pipes passant par ici, je poursuis la marche aléatoire.

  where -- Génère un voisin aléatoire que l'on n'a pas visité
        randomNeighbour = do
          seen <- map fst <$> use seen
          neighs <- M.filterWithKey (\vID _ -> not $ vID `elem` seen) . _eMap <$> use currentNeighs -- On considère un voisin que l'on n'a pas déjà visité
          if (length neighs == 0) then pure Nothing
            else do
          r <- getRandomInt (0,M.size neighs-1)
          pure $ Just $ M.elemAt r neighs
        moveToRandomNeighbour = do
          r  <- randomNeighbour
          case r of
            Nothing -> pure False
            Just (nxtID,_) -> do
              moveTo g nxtID
              randomWalkToMe g (maxLen - 1)
        -- Progresse le long d'un pipe jusqu'à moi
        walkOnPipe (pipeID,(PipeNode prevM nextM _ dir _)) = do
          (me,posID) <- (,) <$> use me <*> use currentID
          if posID == me then pure True
            else case dir of
              NextD -> do moveTo g (fromJust nextM)
                          localPipes <- use (currentVal . pipesT . vPipes)
                          walkOnPipe (pipeID, fromJust $ M.lookup pipeID localPipes)
              PrevD -> do moveTo g (fromJust prevM)
                          localPipes <- use (currentVal . pipesT . vPipes)
                          walkOnPipe (pipeID, fromJust $ M.lookup pipeID localPipes)




