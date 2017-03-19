{-# LANGUAGE TemplateHaskell #-}
module Types.Graph.RoadGraph where
import           Control.Lens
import           Control.Monad
import           Data.Binary
import qualified Data.Map.Strict  as M
import           Data.Maybe
import           Data.Monoid
import           Types.Graph.Type
import           Types.Packets

minEdgeTTL = 120 -- [TODO] Durée avant que l'arc ne puisse timeout

{-| Détails d'un pipe passant par un noeud |-}
data PipeType = Local | Relayed
data Direction = PrevD | NextD
               deriving Eq
data PipeNode = PipeNode {_previous :: Maybe VertexID,
                          _next     :: Maybe VertexID,
                          _pathLen  :: Int, -- Nombre d'arcs d'ici à moi
                          _pathToMe :: Direction, -- Direction à emprunter pour arriver à moi
                          _pipeType :: PipeType} -- Type de pipe (que l'on relaie ou que l'on demande)

data VertexPipes = VertexPipes {_vPipes   :: M.Map PipeID PipeNode}
data VertexT = VertexT {_pipesT :: VertexPipes}

makeLenses ''VertexPipes
makeLenses ''VertexT
instance Monoid VertexPipes where
  mempty = VertexPipes mempty
  v1 `mappend` v2 = over vPipes (<> _vPipes v2) v1
instance Monoid VertexT where
  mempty = VertexT mempty
  vT1 `mappend` vT2@(VertexT _) = over pipesT (<> _pipesT vT2) vT1

data EdgeT = EdgeT {edgeUpTime :: Time}
instance Monoid EdgeT where
  mempty = EdgeT $ (0 :: Time)
  (EdgeT t1) `mappend` (EdgeT t2) = EdgeT (max t1 t2)

type RoadGraph = Graph VertexT EdgeT


deletePipeFromVertex :: PipeID -> VertexT -> VertexT
deletePipeFromVertex pipeID vt = over (pipesT . vPipes) (M.delete pipeID) vt

{-| Retourne la liste des voisins par lesquels passe le pipe |-}
edgesFromPipe :: PipeID -> VertexT -> [VertexID]
edgesFromPipe pipeID vT = case pipeID `M.lookup` view (pipesT . vPipes) vT of
  Nothing -> []
  Just p  -> catMaybes [_previous p, _next p]

{-| Retourne le vertexID du sommet suivant sur le graphe, s'il existe |-}
nextOnPipe :: PipeID ->  VertexT -> Maybe VertexID
nextOnPipe pipeID vT = join $ _next <$> M.lookup pipeID (view (pipesT . vPipes) vT)

{-| Retourne le vertexID du sommet précédent sur le graphe, s'il existe |-}
prevOnPipe :: PipeID -> VertexT -> Maybe VertexID
prevOnPipe pipeID vT = join $ _previous <$> M.lookup pipeID (view (pipesT . vPipes) vT) -- [TODO] Code dupliqué avec nextOnPipe :(

{-| Retourne le vertexID du sommet le plus proche sur le pipe |-}
nearestOnPipe :: PipeID -> VertexT -> Maybe VertexID
nearestOnPipe pipeID vT
  | isNothing pipeNodeM = error $ "nearestOnPipe leads to the an unregistered vertex: pipeID=" ++ show pipeID
  | isJust pipeNodeM = join $ f <$> pipeNodeM
 where pipeNodeM = M.lookup pipeID (view (pipesT . vPipes) vT)
       f :: PipeNode -> Maybe VertexID
       f pNode
         | _pathLen pNode == 0 = Nothing
         | _pathToMe pNode == NextD = _next pNode
         | _pathToMe pNode == PrevD = _previous pNode

{-| Folds on g using the specified operator dir |-}
foldModifyDirM
  :: Monad m =>
     (PipeID -> VertexT ->  Maybe VertexID)
     -> PipeID
     -> (VertexID -> VertexT -> t -> m (t, VertexT))
     -> VertexID
     -> t
     -> RoadGraph
     -> m (t, RoadGraph)
foldModifyDirM dir pipeID f vID tInit g = foldModifyGraphM f' vID tInit g
  where f' vID vT acc = do (acc',vT') <-f vID vT acc
                           pure(acc',vT',dir pipeID vT)
foldDirM
  :: Monad m =>
     (PipeID -> VertexT -> Maybe VertexID)
     -> PipeID
     -> (VertexID -> VertexT -> t -> m t)
     -> VertexID
     -> t
     -> RoadGraph
     -> m t
foldDirM dir pipeID f vID tInit g = fst <$> foldModifyDirM dir pipeID f' vID tInit g
  where f' vID vT acc = do acc' <- f vID vT acc
                           pure (acc',vT)
foldModifyDir
  :: (PipeID -> VertexT -> Maybe VertexID)
     -> PipeID
     -> (VertexID -> VertexT -> t -> (t, VertexT))
     -> VertexID
     -> t
     -> RoadGraph
     -> (t, RoadGraph)
foldModifyDir dir pipeID f vID tInit g = runIdentity $ foldModifyDirM dir pipeID (\x y z -> pure $ f x y z) vID tInit g


foldDir
  :: (PipeID -> VertexT -> Maybe VertexID)
     -> PipeID
     -> (VertexID -> VertexT -> t -> t)
     -> VertexID
     -> t
     -> RoadGraph
     -> t
foldDir dir pipeID f vID tInit g = runIdentity $ foldDirM dir pipeID (\x y z -> pure $ f x y z) vID tInit g


{-| Folds on pipes in left, right or nearest direction |-}
foldModifyNextsM :: (Monad m) => PipeID -> (VertexID -> VertexT -> t -> m (t, VertexT)) -> VertexID -> t -> RoadGraph -> m (t,RoadGraph)
foldModifyNextsM = foldModifyDirM nextOnPipe
foldModifyPrevsM :: (Monad m) => PipeID -> (VertexID -> VertexT -> t -> m (t, VertexT)) -> VertexID -> t -> RoadGraph -> m (t,RoadGraph)
foldModifyPrevsM = foldModifyDirM prevOnPipe
foldModifyNearestM :: (Monad m) => PipeID -> (VertexID -> VertexT -> t -> m (t, VertexT)) -> VertexID -> t -> RoadGraph -> m (t,RoadGraph)
foldModifyNearestM = foldModifyDirM nearestOnPipe
foldNextsM :: (Monad m) => PipeID -> (VertexID -> VertexT -> t -> m t) -> VertexID -> t -> RoadGraph -> m t
foldNextsM = foldDirM nextOnPipe
foldPrevsM :: (Monad m) => PipeID -> (VertexID -> VertexT -> t -> m t) -> VertexID -> t -> RoadGraph -> m t
foldPrevsM = foldDirM prevOnPipe
foldNearestM :: (Monad m) => PipeID -> (VertexID -> VertexT -> t -> m t) -> VertexID -> t -> RoadGraph -> m t
foldNearestM = foldDirM nearestOnPipe

foldModifyNexts = foldModifyDir nextOnPipe
foldModifyPrevs = foldModifyDir prevOnPipe
foldModifyNearest = foldModifyDir nearestOnPipe
foldNexts = foldDir nextOnPipe
foldPrevs = foldDir prevOnPipe
foldNearest = foldDir nearestOnPipe

{-| Retourne la liste des sommets du pipe pipeID à partir du sommet vID |-}
nextsOnPipe :: PipeID -> VertexID -> RoadGraph -> [(VertexID,VertexT)]
nextsOnPipe pipeID vID g = foldNexts pipeID (\v vT acc -> acc ++ [(v,vT)]) vID [] g
{-| Retourne la liste des précédents du pipe pipeID à partir du sommet vID |-}
prevsOnPipe :: PipeID -> VertexID -> RoadGraph -> [(VertexID,VertexT)]
prevsOnPipe pipeID vID g = foldPrevs pipeID (\v vT acc -> (v,vT):acc) vID [] g

nearestsOnPipe :: PipeID -> VertexID -> RoadGraph -> [(VertexID,VertexT)]
nearestsOnPipe pipeID vID g = foldNearest pipeID (\v vT acc -> acc ++ [(v,vT)]) vID [] g
{-| Extrait la route associée au PipeID passant par le sommet spécifié -}
getPipeRoad :: PipeID -> VertexID -> RoadGraph -> [(VertexID,VertexT)]
getPipeRoad pID vID g = prevsOnPipe pID vID g ++ nextsOnPipe pID vID g

{-| Insère le pipe sur la route spécifiée|-}
insertPipe :: VertexID -> PipeType -> PipeID -> [VertexID] -> RoadGraph -> RoadGraph
insertPipe _ _ _ [] g = g
insertPipe me pipeType pipeID road g = addRoad src ((\ (vID,vT) -> ((vID,mempty),vT)) <$> verticesT) g
  where road' = zip computeDirections $ buildRoad road
        -- Détermine les positions et les directions relativement à moi
        computeDirections = let (nxt,prev) = break ((== me) . snd) $ zip [0..] road -- numérotation des sommets et séparation des portions avant et après moi
                            in (set _2 NextD <$> reverse nxt) ++ (set _2  PrevD <$> prev)
        makeVPipe ((pos,dir),(prevM,me,nM)) = VertexPipes $ M.fromList [(pipeID,PipeNode prevM nM pos dir pipeType)]
        makeVT = flip (set pipesT) mempty . makeVPipe
        --makeVT (prevM,me,nM) = mempty{_pipesT = makeVPipe (prevM,me,nM)}
        (src:verticesT) = zip road $ makeVT <$> road'

{-| Supprime le pipe de la route spécifiée (ne modifie pas les arcs) |-}
deletePipe :: PipeID -> VertexID -> RoadGraph -> RoadGraph
deletePipe pipeID vID g = snd $ foldModifyPrevs pipeID delPipe vID () g1
  where delPipe _ vT _ = ((), deletePipeFromVertex pipeID vT)
        (_,g1) = foldModifyNexts pipeID delPipe vID () g

{-| Supprime tous les sommets par lesquels aucun pipe ne passe et n'ayant aucun arc incident qui ne soit frais |-}
cleanRoadGraph :: Time -> RoadGraph -> RoadGraph
cleanRoadGraph cur = filterGraph f
  where f _ vT (Edges edges) = (not $ M.null $ _vPipes (_pipesT vT)) || (or $ fmap (\e -> edgeUpTime e + minEdgeTTL <= cur) edges)

