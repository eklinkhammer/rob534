{-# OPTIONS -Wall #-}
module AStar
       (
         aStar
       ) where

import MyTypes


import Data.Matrix
import Data.PSQueue
import qualified Data.Map.Strict as Map
import Data.Maybe
import Prelude hiding (lookup, null)

maxVel :: Int
maxVel = 2

root2 :: Double
root2 = sqrt 2

-- Consider turning off -Wtype-defaults
maxVelH :: Double
maxVelH = let dMaxVel = fromIntegral maxVel :: Double
              exp2 = 2 :: Integer
          in dMaxVel^exp2 * root2

-- ************************************************
--         Map Helper Functions
-- ************************************************
isPointEmpty :: WorldMap -> State -> Bool
isPointEmpty worldMap state = let x = safeGet i j worldMap
                              in case x of
                                   Nothing    -> False
                                   (Just val) -> val == 0
  where (i,j) = getPos state

-- all Points in between are empty
isPathEmpty :: WorldMap -> State -> State -> Bool
isPathEmpty world (TwoD (x,y))    (TwoD (x',y')) = isPathEmpty world (FourD (x,y) (0,0)) (FourD (x',y') (x' - x, y' - y))
isPathEmpty world (FourD (x,y) _) (FourD (x',y') (dx',dy'))
  | dx' > 1 && dy' > 1  = Prelude.foldl (&&) True (map (isPointEmpty world) (middle : (getNeighbors2D world middle)))
  | dx' > 1 && dy' == 1 = isPointEmpty world (TwoD (x + 1,y)) && isPointEmpty world (TwoD (x + 1, y'))
  | dy' > 1 && dx' == 1 = isPointEmpty world (TwoD (x, y + 1)) && isPointEmpty world (TwoD (x', y + 1))
  | dy' == 1 && dx' == 1 = isPointEmpty world (TwoD (x,y')) && isPointEmpty world (TwoD (x',y))
  | otherwise = True
  where middle = TwoD (x + 1, y + 1)
isPathEmpty _ _ _ = False

getNeighbors :: WorldMap -> State -> [State]
getNeighbors world s@(TwoD _)    = getNeighbors2D world s
getNeighbors world s@(FourD _ _) = getNeighbors4D world s

getNeighbors2D :: WorldMap -> State -> [State]
getNeighbors2D worldMap state =
  let (i,j) = getPos state
      fourConnected = [(i+1,j), (i-1,j), (i, j+1), (i, j-1)]
      goodPos = filter (\(x,y) -> isJust $ safeGet x y worldMap) fourConnected
  in map posToState2D goodPos

getNeighbors4D :: WorldMap -> State -> [State]
getNeighbors4D worldMap s@(TwoD _) = getNeighbors2D worldMap s
getNeighbors4D worldMap s@(FourD (x,y) (dx,dy)) =
  let potentialVels = [(dx + 1, dy), (dx,dy), (dx - 1, dy), (dx, dy + 1), (dx, dy - 1)]
      validVels = filter validVel potentialVels
      connected = map (\(x',y') -> FourD (x + x', y + y') (x', y')) validVels
      pathClear = filter (isPathEmpty worldMap s) connected
  in filter (\st -> let (i,j) = getPos st
                   in isJust $ safeGet i j worldMap) pathClear
                
      

validVel ::  Vel -> Bool
validVel (dx,dy) = dx >= 0 && dy >= 0 && dy <= maxVel && dx <= maxVel

aStar :: WorldMap -> Start -> Goal -> Epsilon -> (Path, NumNodesExpanded)
aStar worldMap start goal epsilon = (reverse $ reconstructPath start goal previous, nodes)
  where
    ((AStar _ _ previous _),nodes) = aStarHelper 0 epsilon worldMap goal (AStar open closed prev g)
    open   = singleton start 0.0
    closed = Map.empty
    prev   = Map.empty
    g      = Map.singleton start 0.0


reconstructPath :: Start -> State -> Previous -> Path
reconstructPath start current previousMap = if start == current then [start]
                                            else current : (reconstructPath start (fromJust $ Map.lookup current previousMap) previousMap)

aStarHelper :: NumNodesExpanded -> Epsilon -> WorldMap -> Goal -> AStar -> (AStar, NumNodesExpanded)
aStarHelper nodes esp m goal (AStar open closed prev g) = if null open
                                                          then (AStar open closed prev g, nodes)
                                                          else aStarHelper (nodes + 1) esp m goal newAStar
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
                 else handleNeighbors esp goal currentNode free_neighbors (AStar open' closed' prev g)

handleNeighbors :: Epsilon -> Goal -> State -> [State] -> AStar -> AStar
handleNeighbors _ _    _ []     astar                        = astar
handleNeighbors e goal s (x:xs) a@(AStar open closed prev g) = if isNothing $ Map.lookup x closed
                                                               then handleNeighbors e goal s xs newAStar
                                                               else handleNeighbors e goal s xs a
  where
    newAStar = AStar open' closed prev' g'
    distCurrentToNeighbor = distanceBetweenNeighbors s x
    distFromStartToX = distCurrentToNeighbor + (fromJust $ Map.lookup s g)
    gScore = computeGScore e goal distFromStartToX x
    open' = if isNothing $ lookup x open then insert x gScore open else open
    (prev', g') = if distFromStartToX < Map.findWithDefault infinity x g
                  then (Map.insert x s prev, Map.insert x gScore g)
                  else (prev, g)
-- Manhattan distance
distanceBetweenNeighbors :: State -> State -> Double
distanceBetweenNeighbors state1 state2 = let ((x1,y1), (x2,y2)) = (getPos state1, getPos state2)
                                         in fromIntegral $ (y2 - y1) + (x2 - x1)

-- Estimated distance between the two states. Second state assumed to be goal
--  state, but not necessarily
--  Third pattern match may be inadmissable. Included so I can keep my -Wall
heuristic :: State -> State -> DistanceToGoal
heuristic (TwoD pos1)    (TwoD pos2)    = heuristic2D pos1 pos2
heuristic s1@(FourD _ _) s2@(FourD _ _) = heuristic4D s1 s2
heuristic a              b              = heuristic2D (getPos a) (getPos b)

heuristic2D :: Pos -> Pos -> DistanceToGoal
heuristic2D (x,y) (a,b) = sqrt (squareDif b y + squareDif a x)
  where squareDif m n = let dif = fromIntegral (m - n) :: Double
                            exp2 = 2 :: Integer
                        in dif^exp2

-- The eucliean distance between two states, traversed at maximum velocity
heuristic4D :: State -> State -> DistanceToGoal
heuristic4D state1 state2 = let dist = heuristic2D (getPos state1) (getPos state2)
                            in dist / maxVelH
                               
computeGScore :: Epsilon -> Goal -> DistanceFromStart -> State -> DistanceToGoal
computeGScore esp goal dist node = dist + (esp * heuristic node goal)
