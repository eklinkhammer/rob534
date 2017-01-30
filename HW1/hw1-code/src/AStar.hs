module AStar
       (
         aStar
       ) where

import Display
import MyTypes


import Data.Matrix
import Data.PSQueue
import qualified Data.Map.Strict as Map
import Data.Maybe
import Prelude hiding (lookup, null)


-- ************************************************
--         Map Helper Functions
-- ************************************************
isPointEmpty :: WorldMap -> Pos -> Bool
isPointEmpty map pos = (map ! pos) == 0

getNeighbors :: WorldMap -> Pos -> [Pos]
getNeighbors map (i,j) =
  let fourConnected = [(i+1,j), (i-1,j), (i, j+1), (i, j-1)]
  in filter (\(x,y) -> isJust $ safeGet x y map) fourConnected

     
aStar :: WorldMap -> Start -> Goal -> Path
aStar worldMap start goal = reverse $ reconstructPath start goal previous
  where
    (AStar _ _ previous _) = aStarHelper worldMap goal (AStar open closed prev g)
    open = singleton start 0.0
    closed = Map.empty
    prev   = Map.empty
    g      = Map.singleton start 0.0


reconstructPath :: Start -> Pos -> Previous -> Path
reconstructPath start current previousMap = if start == current then [start]
                                            else current : (reconstructPath start (fromJust $ Map.lookup current previousMap) previousMap)

aStarHelper :: WorldMap -> Goal -> AStar -> AStar
aStarHelper m goal (AStar open closed prev g) = if null open
                                                then AStar open closed prev g
                                                else aStarHelper m goal newAStar
    where
      -- Pop off the node with the lowest priority
      (currentNodeBinding, openSet) = fromJust $ minView open
      currentNode = key currentNodeBinding
      -- If the node with the lowest priority is the goal, you don't recurse
      open' = if currentNode == goal then empty else openSet
      -- Add the current node to the closed set and remove from the open set
      closed' = Map.insert currentNode True closed
      neighbors = getNeighbors m currentNode
      free_neighbors = filter (isPointEmpty m) neighbors 
      newAStar = if currentNode == goal
                 then AStar empty closed prev g
                 else handleNeighbors goal currentNode free_neighbors (AStar open' closed' prev g)

handleNeighbors :: Goal -> Pos -> [Pos] -> AStar -> AStar
handleNeighbors goal s [] astar = astar
handleNeighbors goal s (x:xs) a@(AStar open closed prev g) = if isNothing $ Map.lookup x closed
                                                             then handleNeighbors goal s xs newAStar
                                                             else handleNeighbors goal s xs a
  where
    newAStar = AStar open' closed prev' g'
    distFromStartToX = 1.0 + (fromJust $ Map.lookup s g)
    gScore = computeGScore goal distFromStartToX x
    open' = if isNothing $ lookup x open then insert x gScore open else open
    (prev', g') = if distFromStartToX < Map.findWithDefault infinity x g
                  then (Map.insert x s prev, Map.insert x gScore g)
                  else (prev, g)

heuristic :: Pos -> Pos -> DistanceToGoal
heuristic (x,y) (a,b) = sqrt (squareDif b y + squareDif a x)
  where squareDif m n = (fromIntegral (m - n))^2

computeGScore :: Goal -> DistanceFromStart -> Pos -> DistanceToGoal
computeGScore goal dist node = dist + heuristic node goal
