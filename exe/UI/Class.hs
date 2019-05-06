{-# LANGUAGE TemplateHaskell #-}
module UI.Class where

import Brick
--import Data.Default
import Graphics.Vty
import Control.Concurrent.Chan
import Graphics.Vty.Input.Events
import qualified Data.Array as A
import Data.List

import Control.Lens

type ClientWidget = A.Array (Int,Int) (Widget String)

buildClientWidget :: ClientWidget -> Widget String
buildClientWidget cw = vBox [hBox [padAll 2 $ cw A.! (i,j) |j <- [1..nbCol]]  | i <- [1..nbLi]]
  where (_,(nbLi,nbCol)) = A.bounds cw

data ClientUI = ClientUI {_cuiWidgets :: A.Array Int (String,ClientWidget),
                          _cuiCurrent :: Int } 

makeLenses ''ClientUI 
data UIEvent = UIKey Event
             | UIModifier (ClientUI -> ClientUI)
type ClientWidgetModifier = (Int, Int) -> String -> UIEvent
onUIKey cui e = case e of
                  KLeft -> continue $ cui {_cuiCurrent = decCur cur}
                  KRight -> continue $ cui {_cuiCurrent = incrCur cur}
                  KEsc -> halt cui
                  KChar 'q' -> halt cui
                  otherwise -> continue cui
    where (mi,ma) = A.bounds (_cuiWidgets cui)
          cur = _cuiCurrent cui
          incrCur i = if i+1 > ma then mi else i+1
          decCur i = if i-1 < mi then ma else i-1

drawCurrentWidget cui = vBox [str names,buildClientWidget cur]
    where cur = getCurrentWidget cui
          curInd = _cuiCurrent cui
          drawSel i (x,_)
            | i == curInd = "[" ++ x ++ "]"
            | otherwise = x
          --names = concat $ intersperse "\t" $  (uncurry drawSel <$> (A.assocs $ cuiWidgets cui))
          names = unwords $ map (uncurry drawSel) (A.assocs $ _cuiWidgets cui) --(A.assocs $ _cuiWidgets cui)

{-
type ClientWidget = ([Widget], String)

type ClientApp = App ClientUI UIEvent
-}


newClientApp :: App ClientUI UIEvent String
newClientApp = App (\cui -> [drawCurrentWidget cui]) --(drawCurrentWidget cui : map fst ( A.elems (_cuiWidgets cui))))
                   (\_ _ -> Nothing)
                   eventHandler
                   pure 
                   (\_ -> attrMap mempty [])
    where eventHandler :: ClientUI -> BrickEvent String UIEvent -> EventM String (Next ClientUI)
          eventHandler cui (AppEvent (UIKey (EvKey e _) ) ) = onUIKey cui e 
          eventHandler cui (AppEvent (UIModifier modifier) ) = continue $ modifier cui
          eventHandler cui _ = continue cui



{-| Prend en argument le nombre de fenetres et leur noms.
    Retourne une "app" contenant des fenetres vides, ainsi que des modifiers pour
    créer un évent modifiant l'écran pour écrire la string (à push dans le Chan correspondant)
 -}
--newClientUI :: Int -> [String] -> (ClientUI, [String -> String -> UIEvent])
--newClientUI :: Int -> [String] -> (ClientUI, [String -> String -> UIEvent])
--newClientUI nbWidgets names  = (ret, map updateWidget [1..])
--    where tab = A.listArray (1,nbWidgets) [(str "",ni) | ni <- names]
--          ret = ClientUI tab 1



newClientUI :: [(String,(Int,Int))] -> (ClientUI,[ClientWidgetModifier])
newClientUI  grids  = (ClientUI (A.listArray (1,nbGrids) $ zip names (mkClientWidget <$> sizes)) 1, uiModifier <$> [1..nbGrids])
   where mkClientWidget (nbLi,nbCol) = A.array ((1,1),(nbLi,nbCol)) $ [((i,j),str "") | i <- [1..nbLi], j <- [1..nbCol]]
         nbGrids = length grids
         (names,sizes) = unzip grids
        


{-
updateWidget :: Int -> String -> String -> UIEvent
updateWidget i name string = UIModifier $ over cuiWidgets (A.// [(i,(str string,name))]) 
-}

{-| Modifie la grille considérée |-}
updateWidget :: ClientWidget -> (Int,Int) -> String -> ClientWidget
updateWidget w (i,j) val = w A.// [((i,j), str val)]

{-| Modifie la grille de l'indice i |-}
uiModifier' :: Int -> (Int,Int) -> String -> ClientUI -> ClientUI
uiModifier' i coord val cui = over cuiWidgets f cui
  where (name,targetGrid) = _cuiWidgets cui A.! i
        f t = t A.// [(i,(name,updateWidget targetGrid coord val))]

uiModifier :: Int -> ClientWidgetModifier
uiModifier i coord val = UIModifier $ uiModifier' i coord val

{- tools -}
getCurrentWidget :: ClientUI -> ClientWidget
getCurrentWidget cui = snd $ _cuiWidgets cui A.! _cuiCurrent cui




{-type TestA = App Test EventT
newTestA = App ((:[]) . vBox.wid) (\x y -> Nothing) (\x y -> continue x) (\x -> pure x) def (pure EventT)
-}



