{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module Graph.Type where

import           Control.Lens
import           Control.Monad.Identity
import           Data.List
import qualified Data.Map.Strict        as M
import           Data.Maybe
import           Data.Monoid
import           Types


data VertexID = VertexID Int
                deriving (Eq,Ord,Show)

{-| Les arcs incidents à chaque sommet sont stockés dans une Map |-}
data Edges edge =  Edges {_eMap :: M.Map VertexID edge}
makeLenses ''Edges

{-| Un graphe est une map qui à chaque sommet associe sa valeur et ses destinataires |-}
data Graph vertex edge = Graph {_vMap :: M.Map VertexID (vertex,Edges edge)}
makeLenses ''Graph
type G v e = (Monoid v, Monoid e) => Graph v e


{-| Permet de fusionner deux tables d'arcs. Les valeurs présentes
   dans les deux maps sont <> |-}
instance (Monoid e) => Monoid (Edges e) where
  mempty = Edges $ M.empty
  e1 `mappend` e2 = over eMap (M.unionWith (<>) $ _eMap e2) e1

{-| Permet de fusionner deux graphes. Les sommets présents dans les deux graphes
   voient leur valuation et tables d'arc fusionnés également |-}
instance (Monoid v, Monoid e) => Monoid (Graph v e) where
  mempty = Graph $ M.empty
  g1 `mappend` g2 = over vMap (M.unionWith (<>) $ _vMap g2) g1

{-| Ajoute un noeud partiel d'une route avec son estimation (merge les estmations si l'entrée existe)|-}
addRoad' :: Maybe (VertexID,edge) -- l'arc incident à moi (s'il existe)
        -> (VertexID,vertex) -- Moi (et mon estimation)
        -> Maybe (VertexID, edge) -- l'arc sortant de moi (s'il existe)
        -> G vertex edge -> G vertex edge
addRoad' prevM (me,meval) nxtM g = g <> newGraph
  where newEdges = Edges $ M.fromList $ catMaybes [prevM,nxtM]
        newGraph = Graph $ M.fromList $ [(me,(meval,newEdges))]

{-| Applique une fonction pour modifier la valeur de chaque sommet et de chaque arrête d'un noeud partiel d'une route.
    Permet également de briser des arcs.
|-}
editRoad' :: (vertex -> vertex)  -- Fonction appelée pour modifier la valeur de chaque sommet de la route
         -> (edge -> Maybe edge) -- Fonction appelée pour modifier la valeur d'un arc de la route (Just edge) ou briser l'arc (Nothing)
         -> Maybe VertexID -- Mon précédent (s'il existe)
         -> VertexID -- Moi
         -> Maybe VertexID -- Mon suivant (s'il existe)
         ->  G vertex edge
         -> G vertex edge

editRoad' editV editE prevM me nxtM g = over vMap (M.adjust f me) g
  where f (vval,(Edges edges)) = (editV vval, -- mise à jour de la valeur du sommet
                                  Edges $ foldr (M.update editE) edges $ catMaybes [prevM,nxtM]) -- mise à jour de la valeur des arcs prec et nxt s'ils existent

{-| Détermine les triplets (precedent,sommet,suivant) pour chaque sommet d'une route |-}

buildRoad :: [a] -> [(Maybe a, a, Maybe a)]
buildRoad l = zip3 (Nothing:l') l (tail l' ++ [Nothing]) -- <3<3
  where l' = Just <$> l


{-| Ajoute une route ((sommetID,valeur de l'arc),valeur du sommet) au graphe.
    ATTENTION; on ne vérifie pas que les arcs coincident.
|-}
addRoad :: (VertexID,vertex) -- Origine de la route
        -> [((VertexID,edge),vertex)] -- voisins successifs (id, estimation du sommet, estimation de l'arc)
        -> G vertex edge -> G vertex edge
addRoad _ [] g = g
addRoad src road@(((firstID,firstE),firstV):_) g = g' <> mconcat (mkVertex <$> r) --foldr f g' r
  where r  = (\ (prevM,((meID,incE),meval),nxtEM) -> let prevEM =  if isJust prevM then (set _2 incE . fst <$>  prevM) else Just (fst src,incE)
                                                    in (prevEM,(meID,meval),fst <$> nxtEM)) <$> buildRoad road

        mkVertex :: (Maybe (VertexID,edge),(VertexID,vertex), Maybe (VertexID,edge)) -> G vertex edge
        mkVertex (prevEM,me,nxtEM) =  addRoad' prevEM me nxtEM mempty
        g' = addRoad' Nothing src (Just $ (firstID,firstE)) g -- J'ajoute le premier arc implicite (src -> e1 -> r1)

{-| Met à jour les valeurs de tous les sommets et de tous les arcs d'une route.
    Les arcs peuvent être brisés, mais aucun traitement supplémentaire n'est fait.
|-}
editRoad :: (vertex -> vertex) -> (edge -> Maybe edge)
               -> [VertexID]
               -> G vertex edge -> G vertex edge
editRoad editV editE road g = foldr f g r
  where r = buildRoad road
        f (prevM,me,nxtM) = editRoad' editV editE prevM me nxtM

{-| Supprime les arcs de la route |-}
deleteRoad :: (vertex -> vertex) -> [VertexID] -> G vertex edge -> G vertex edge
deleteRoad editV road g = editRoad editV (pure Nothing) road g


{-| Permet d'itérer une fonction sur les sommets du graphe, de les modifier
    et d'accumuler ses résultats
   [TODO] Mettre le prochain vertex dans le type de retour de la fonction accumulée (au lieu d'utiliser un itérateur différent)|-}

foldModifyGraphM :: (Monad m)
                 => (VertexID -> vertex -> t -> m (t,vertex,Maybe VertexID)) -- Prend en argument le VertexID courant, la valeur associée
                                                                        -- et l'accumulateur et retourne (accumulateur,nouvelle valeur du sommet et peut être le sommet suivant)
                                                                        -- Si le sommet suivant vaut Nothing, on s'arrête
                 -> VertexID
                 -> t
                 -> Graph vertex edge
                 -> m (t,Graph vertex edge)
foldModifyGraphM f vID acc g
  | isNothing vTM = error "foldModifyGraph Itération vers un vID n'existant pas"
  | otherwise = do
      (acc',vT',nextIDM) <- f vID (fromJust vTM) acc
      let g' = over vMap (M.adjust (set _1 vT') vID) g
      case nextIDM of
        Nothing     -> pure (acc',g')
        Just nextID -> foldModifyGraphM f nextID acc' g'
  where vTM = fst <$> M.lookup vID (_vMap g)


{-| Itère une fonction sur un graphe et accumule ses résultats, mais sans le modifier. |-}
foldGraphM :: (Monad m)
           => (VertexID -> vertex -> t -> m (t,Maybe VertexID))
           -> VertexID
           -> t
           -> Graph vertex edge
           -> m t
foldGraphM f vID acc g = fst <$> foldModifyGraphM f' vID acc g
  where f' vID vT acc = do (acc',nextIDM) <- f vID vT acc
                           pure (acc,vT,nextIDM)

{-| Retourne le sous graphe des noeuds satisfaisant le prédicat.
    Le prédicat prend, pour chaque sommet: - vertexID
                                           - valuation du sommet
                                           - voisins du sommet
|-}
filterGraph :: (VertexID -> vertex -> Edges edge -> Bool) -- Vrai si le noeud doit être conservé
              -> Graph vertex edge
              -> Graph vertex edge

filterGraph f g = Graph $ M.fromList $ updateEntry <$> toKeep
  where nodes = M.toList $ _vMap g
        keepP k _ = k `elem` (fst <$> toKeep)
        (toKeep,toDel) = partition (\(vID,(vVal,edges)) -> f vID vVal edges) nodes
        updateEntry (vID,(vVal,edges)) = (vID,(vVal, over eMap (M.filterWithKey keepP) edges))

{-| itère une fonction sur un graphe et accumule les résultats (pour un comportement monadique, voir @foldModifyGraphM -}
foldModifyGraph
  ::  (VertexID -> vertex -> t -> (t,vertex,Maybe VertexID)) -- Fonction accumulant le précédent résultat avec le sommet considéré
     -> VertexID -- Sommet initial
     -> t -- Valeur initiale de l'accumulateur
     -> Graph vertex edge -- Graphe
     -> (t,Graph vertex edge) -- l'accumulateur final et le graphe modifié
foldModifyGraph f vID acc g = runIdentity $ foldModifyGraphM (\a b c -> pure $ f a b c) vID acc g
{-| Itère une fonction sur le graphe et accumule ses résultats
    sans la modifier (non monadique)|-}
foldGraph ::
          (VertexID -> vertex -> t -> (t,Maybe VertexID))
          -> VertexID
          -> Graph vertex edge
          -> t
          -> t
foldGraph f vID g acc = runIdentity $ foldGraphM (\a b c -> pure $ f a b c) vID acc g





