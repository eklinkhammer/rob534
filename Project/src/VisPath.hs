module VisPath
       (
         visPath
       ) where

import ConstructVis

import Data.Graph.AStar
import qualified Data.HashSet as S
import qualified Data.Map.Strict as Map
import System.IO.Unsafe

type Start = (Double, Double)
type End = (Double, Double)
type Obstacles = [[(Double,Double)]]


visPath :: Double -> Double -> Start -> End -> Obstacles -> Maybe [(Double, Double)]
visPath r n start end obs =
  let visGraph = constructVisGraph r n [start,end] obs Map.empty
  in aStar (neighbors visGraph) distance (distance end) (isGoal end) start


distance :: (Double, Double) -> (Double, Double) -> Double
distance (a,b) (x,y) = sqrt $ (x - a)^2 + (y - b)^2

isGoal :: (Double, Double) -> (Double, Double) -> Bool
isGoal x y = x == y

neighbors v p = v Map.! p

