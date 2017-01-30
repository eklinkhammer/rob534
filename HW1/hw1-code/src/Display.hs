module Display
       (
         displayPathOnMap
       , showMapWithPath
       ) where

import MyTypes

import Data.Matrix
import Data.PSQueue
import qualified Data.Map.Strict as Map

showMapWithPath :: WorldMap -> Path -> IO ()
showMapWithPath world = putStrLn . show . displayPathOnMap world

displayPathOnMap :: WorldMap -> Path -> Matrix String
displayPathOnMap worldMap path = Prelude.foldl updateDisplay stringMatrix path
  where
    stringMatrix = fmap show worldMap
    

updateDisplay :: Matrix String -> Pos -> Matrix String
updateDisplay mat pos = setElem "X" pos mat
