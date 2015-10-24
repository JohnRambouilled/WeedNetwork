{-# LANGUAGE TemplateHaskell #-}
module UI.Class where

import Brick
import Data.Default
import Graphics.Vty
import Control.Concurrent.Chan
import Graphics.Vty.Input.Events
import qualified Data.Array as A
import Data.List

import Control.Lens


type ClientWidget = (Widget, String)
data ClientUI = ClientUI {_cuiWidgets :: A.Array Int ClientWidget,
                          _cuiCurrent :: Int } 
makeLenses ''ClientUI 
data UIEvent = UIKey Event
             | UIModifier (ClientUI -> ClientUI)

type ClientApp = App ClientUI UIEvent


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

drawCurrentWidget cui = vBox [str names,fst cur]
    where cur = getCurrentWidget cui
          curInd = _cuiCurrent cui
          drawSel i (_,x)
            | i == curInd = "[" ++ x ++ "]"
            | otherwise = x
          --names = concat $ intersperse "\t" $  (uncurry drawSel <$> (A.assocs $ cuiWidgets cui))
          names = unwords $ map (uncurry drawSel) (A.assocs $ _cuiWidgets cui)

newClientApp :: App ClientUI UIEvent
newClientApp = App (\cui -> (drawCurrentWidget cui : map fst ( A.elems (_cuiWidgets cui))))
                   (\_ _ -> Nothing)
                   eventHandler
                   pure
                   def
                   UIKey
    where eventHandler cui (UIKey (EvKey e _) ) = onUIKey cui e 
          eventHandler cui (UIModifier modifier) = continue $ modifier cui
          eventHandler cui _ = continue cui



{-| Prend en argument le nombre de fenetres et leur noms.
    Retourne une "app" contenant des fenetres vides, ainsi que des modifiers pour
    créer un évent modifiant l'écran pour écrire la string (à push dans le Chan correspondant)
 -}
newClientUI :: Int -> [String] -> (ClientUI, [String -> String -> UIEvent])
newClientUI nbWidgets names  = (ret, map updateWidget [1..])
    where tab = A.listArray (1,nbWidgets) [(str "",ni) | ni <- names]
          ret = ClientUI tab 1


updateWidget :: Int -> String -> String -> UIEvent
updateWidget i name string = UIModifier $ over cuiWidgets (A.// [(i,(str string,name))]) 

{- tools -}
getCurrentWidget :: ClientUI -> ClientWidget
getCurrentWidget cui = _cuiWidgets cui A.! _cuiCurrent cui



{-type TestA = App Test EventT
newTestA = App ((:[]) . vBox.wid) (\x y -> Nothing) (\x y -> continue x) (\x -> pure x) def (pure EventT)
-}



