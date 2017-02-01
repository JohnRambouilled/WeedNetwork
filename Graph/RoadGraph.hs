{-# LANGUAGE TemplateHaskell #-}
module Graph.RoadGraph where
import           Control.Lens
import           Control.Monad
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Graph.Type
import           Types
{-| Détails d'un pipe passant par un noeud |-}
data PipeNode = PipeNode {_previous :: Maybe VertexID,
                          _next     :: Maybe VertexID}


data VertexPipes = VertexPipes {_vPipes :: M.Map PipeID PipeNode}
data VertexT = VertexT {_pipesT :: VertexPipes}

makeLenses ''VertexPipes
makeLenses ''VertexT
instance Monoid VertexPipes where
  mempty = VertexPipes mempty
  v1 `mappend` v2 = over vPipes (<> _vPipes v2) v1
instance Monoid VertexT where
  mempty = VertexT mempty
  vT1 `mappend` vT2 = over pipesT (<> _pipesT vT2) vT1

data EdgeT = EdgeT
instance Monoid EdgeT where
  mempty = EdgeT
  _ `mappend` _ = EdgeT

type RoadGraph = Graph VertexT EdgeT


deletePipeFromVertex :: PipeID -> VertexT -> VertexT
deletePipeFromVertex pipeID vt = over (pipesT . vPipes) (M.delete pipeID) vt

{-| Retourne la liste des voisins par lesquels passe le pipe |-}
edgesFromPipe :: PipeID -> VertexT -> [VertexID]
edgesFromPipe pipeID vT = case pipeID `M.lookup` view (pipesT . vPipes) vT of
  Nothing -> []
  Just p  -> catMaybes [_previous p, _next p]

{-| Retourne le vertexID du sommet suivant sur le graphe, s'il existe |-}
nextOnPipe :: PipeID -> VertexT -> Maybe VertexID
nextOnPipe pipeID vT = join $ _next <$> M.lookup pipeID (view (pipesT . vPipes) vT)

{-| Retourne le vertexID du sommet précédent sur le graphe, s'il existe |-}
prevOnPipe :: PipeID -> VertexT -> Maybe VertexID
prevOnPipe pipeID vT = join $ _previous <$> M.lookup pipeID (view (pipesT . vPipes) vT) -- [TODO] Code dupliqué avec nextOnPipe :(



{-| Applique une fonction sur les suivants du sommet concerné sur le pipe et accumule ses résultats tout en modifiant
    les estimations de chaque sommet |-}
foldModifyNexts :: PipeID
                -> (VertexID -> VertexT -> t -> (t,VertexT))
                -> VertexID
                -> RoadGraph
                -> t
                -> (t,RoadGraph)
foldModifyNexts pipeID f vID g tInit = foldModifyGraph (nextOnPipe pipeID) f vID g tInit

{-| Applique une fonction sur les précédents du sommet concerné sur le pipe et accumule ses résultats tout en modifiant
    les estimations de chaque sommet |-}
foldModifyPrevs
  :: PipeID
     -> (VertexID -> VertexT -> t -> (t, VertexT))
     -> VertexID
     -> RoadGraph
     -> t
     -> (t, RoadGraph)
foldModifyPrevs pipeID f vID g tInit = foldModifyGraph (prevOnPipe pipeID) f vID g tInit

{-| Applique une fonction sur les suivants du sommet concerné sur le pipe et accumule ses résultats. |-}
foldNexts
  :: PipeID
     -> (VertexID -> VertexT -> t -> t)
     -> VertexID
     -> Graph VertexT edge
     -> t
     -> t
foldNexts pipeID = foldGraph (nextOnPipe pipeID)

{-| Applique une fonction sur les précédents du sommet concerné sur le pipe et accumule ses résultats. |-}
foldPrevs pipeID = foldGraph (prevOnPipe pipeID)


{-| Retourne la liste des sommets du pipe pipeID à partir du sommet vID |-}
nextsOnPipe :: PipeID -> VertexID -> Graph VertexT edge -> [(VertexID,VertexT)]
nextsOnPipe pipeID vID g = foldNexts pipeID (\v vT acc -> acc ++ [(v,vT)]) vID g []
{-| Retourne la liste des précédents du pipe pipeID à partir du sommet vID |-}
prevsOnPipe :: PipeID -> VertexID -> Graph VertexT edge -> [(VertexID,VertexT)]
prevsOnPipe pipeID vID g = foldPrevs pipeID (\v vT acc -> (v,vT):acc) vID g []


{-| Extrait la route associée au PipeID passant par le sommet spécifié -}
getPipeRoad :: PipeID -> VertexID -> RoadGraph -> [(VertexID,VertexT)]
getPipeRoad pID vID g = prevsOnPipe pID vID g ++ nextsOnPipe pID vID g

{-| Insère le pipe sur la route spécifiée|-}
insertPipe :: PipeID -> [VertexID] -> RoadGraph -> RoadGraph
insertPipe pipeID road g = addRoad src ((\ (vID,vT) -> ((vID,mempty),vT)) <$> verticesT) g
  where road' = buildRoad road
        makeVPipe (prevM,me,nM) = VertexPipes $ M.fromList [(pipeID,PipeNode prevM nM)]
        makeVT = flip (set pipesT) mempty . makeVPipe
        --makeVT (prevM,me,nM) = mempty{_pipesT = makeVPipe (prevM,me,nM)}
        (src:verticesT) = zip road $ makeVT <$> road'

{-| Supprime le pipe de la route spécifiée (ne modifie pas les arcs) |-}
deletePipe :: PipeID -> VertexID -> RoadGraph -> RoadGraph
deletePipe pipeID vID g = snd $ foldModifyPrevs pipeID delPipe vID g1 ()
  where delPipe _ vT _ = ((), deletePipeFromVertex pipeID vT)
        (_,g1) = foldModifyNexts pipeID delPipe vID g ()
