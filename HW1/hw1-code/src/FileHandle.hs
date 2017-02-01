module FileHandle
       (
         fileToMap
       , mapToFile
       , displayPathOnMap
       ) where

import MyTypes

import Data.Matrix
import Data.PSQueue
import qualified Data.Map.Strict as Map

import Graphics.Pgm
import Data.Array.Unboxed
import Data.Either
import Data.Word

showMapWithPath :: WorldMap -> Path -> IO ()
showMapWithPath world = putStrLn . show . displayPathOnMap world

displayPathOnMap :: WorldMap -> Path -> WorldMap
displayPathOnMap worldMap path = Prelude.foldl updateDisplay worldMap path
    

updateDisplay :: Matrix Int -> State -> Matrix Int
updateDisplay mat state = setElem (stateToInt state) (getPos state) mat

stateToInt :: State -> Int
stateToInt (TwoD _)           = 128 -- All Two d paths are uniform grey
stateToInt (FourD _ (dx, dy)) = min 225 (64 * (dx + dy)) -- Assuming a max speed of 2

mapToFile :: String -> WorldMap -> IO ()
mapToFile fileName world = arrayToFile fileName (mapToArray world)

mapToArray :: WorldMap -> UArray (Int,Int) Word16
mapToArray world = let (rows, cols) = (nrows world, ncols world)
                       lowest = (0,0)
                       highest = (rows - 1,cols - 1)
                       arr = listArray (lowest, highest) (Data.Matrix.toList world)
                   in amap intToPgmWord arr

intToPgmWord :: Int -> Word16
intToPgmWord 0 = fromIntegral 255
intToPgmWord 1 = fromIntegral 0
intToPgmWord n = fromIntegral n

openFile :: String -> IO (UArray (Int, Int) Int)
openFile string = do
  m <- pgmsFromFile string
  let (Right arrs) = m
  return $ head arrs

arrayToMap :: UArray (Int, Int) Int -> WorldMap
arrayToMap arr = let onesAndZeros = amap (\x -> if x == 255
                                                then 0
                                                else 1) arr
                     ((lowerx, lowery),(upperx,uppery)) = bounds arr
                 in Data.Matrix.fromList (upperx - lowerx + 1) (uppery - lowery + 1) (elems onesAndZeros)

fileToMap :: String -> IO WorldMap
fileToMap f = openFile f >>= return . arrayToMap
