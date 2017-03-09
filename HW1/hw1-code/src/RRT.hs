module RRT
  (
    rrt
  , samples
  ) where


import MyTypes
import System.Random
import Data.Matrix
import Data.Maybe
import Data.List

import qualified Data.Map.Strict as Map

type G = Map.Map State State

isPointEmpty :: WorldMap -> State -> Bool
isPointEmpty worldMap state = let x = safeGet i j worldMap
                              in case x of
                                   Nothing    -> False
                                   (Just val) -> val == 0
  where (i,j) = getPos state

  
samples :: RandomGen g => WorldMap -> g -> [State]
samples worldMap g = let (rows, cols) = (nrows worldMap, ncols worldMap)
                         (gen1, gen2) = split g
                         xs = randomRs (0, rows) gen1
                         ys = randomRs (0, cols) gen2
                     in filter (isPointEmpty worldMap) $ map posToState2D (zip xs ys)

rrt :: RandomGen g => WorldMap -> g -> Start -> Goal -> Int -> Path
rrt worldMap gen start goal k =
  let graph = Map.singleton start start
      randomPoints = samples worldMap gen
      finalGraph = rrtHelper worldMap start goal randomPoints k graph
  in reverse $ reconstructPath start (getNearest finalGraph goal) finalGraph
                                   
rrtHelper :: WorldMap -> Start -> Goal -> [State] -> Int -> G -> G
rrtHelper _ _ _ _ 0 g = g
rrtHelper world start goal (x:xs) k graph =
  let rPoint = if 25 `rem` k == 0 then goal else x
      (graph',done) = considerPoint world goal graph rPoint
  in if done then graph' else rrtHelper world start goal xs (k - 1) graph'
  
-- If a point can be added to the graph, add it.
considerPoint :: WorldMap -> Goal -> G -> State -> (G, Bool)
considerPoint worldMap goal graph qrand =
  let nearestVertex = getNearest graph qrand
      withinBound   = getPoint nearestVertex qrand
  in if (not (Map.member withinBound graph)) && validPathTo worldMap nearestVertex withinBound
     then let isGoal = nearGoal withinBound goal
          in (Map.insert withinBound nearestVertex graph, isGoal)
     else (graph, False)


-- constraint - graph is not empty
-- I could use a data structure that does this, but I'm going to convert to a list O(n)
-- and then sort it. Sorting should be done in O(n) time (hopefully). No real idea.
getNearest :: G -> State -> State
getNearest graph state = let listMap = Map.toList graph
                             distances = map (distance state . fst) listMap
                             distAndPoints = zip distances listMap
                             sorted = sortOn fst distAndPoints
                         in fst $ snd $ head $ sorted
          
-- Returns the point in the direction of pt2 within x of pt1
getPoint :: State -> State -> State
getPoint s1 = last . getPathLinear s1

validPathTo :: WorldMap -> State -> State -> Bool
validPathTo worldMap inG rand = foldl (&&) True $ map (isPointEmpty worldMap) (getPathLinear inG rand)

nearGoal :: State -> Goal -> Bool
nearGoal state goal = let ((x,y),(a,b)) = (getPos state, getPos goal)
                      in abs (b - y) < 1 && abs (a - x) < 1

-- Not a full line cover, but conceptually easier
-- step 1 in x, then step in slope. Add both points
getPathLinear :: State -> State -> Path
getPathLinear state1 state2 =
  let ((x,y),(x',y')) = (getPos state1, getPos state2)
      dy' = (fromIntegral $ y' - y)
      dx' = (fromIntegral $ x' - x)
      dx  = dx' / (dx' + dy')
      dy  = dy' / (dx' + dy')
  in map posToState2D [(x + round (dx), y + round (dy)), (x + round (2 * dx), y + round (2 * dy))]

distance :: State -> State -> Double
distance s s2 = let ((x,y),(a,b)) = (getPos s, getPos s2)
                in sqrt (squareDif b y + squareDif a x)
  where squareDif m n = let dif = fromIntegral (m - n) :: Double
                            exp2 = 2 :: Integer
                        in dif^exp2

reconstructPath :: Start -> State -> Previous -> Path
reconstructPath start current previousMap = if start == current then [start]
                                            else current : (reconstructPath start (fromJust $ Map.lookup current previousMap) previousMap)
