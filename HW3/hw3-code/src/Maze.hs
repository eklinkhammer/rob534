module Maze
  (
    Maze (..)
  , MazeLoc (..)
  , Actor (..)
  , Move (..)
  , getReward
  , validDirs
  , getMoveCell
  , createMaze
  , setActor
  , getCell
  , move
  , getIndex
  ) where

import System.Random
import Data.Maybe
import Data.Matrix

data Move = North | South | East | West deriving (Show, Eq)

instance Random Move where
  random g = let (x,y) = (1,4) :: (Int, Int)
                 (i, g') = randomR (x,y) g
             in if i == 1 then (North, g')
                else if i == 2 then (South, g')
                     else if i == 3 then (East, g')
                          else (West, g')
  randomR _ g = random g

data Maze = Maze
            {
              _start :: MazeLoc
            , _maze :: Matrix MazeLoc
            , _robot :: Maybe Actor
            , _movingReward :: Maybe Actor
            }

instance Show Maze where
  show = show . showMaze

data MazeLoc = Cell
               {
                 _adj :: [Bool]
               , _cReward :: Double
               , _cLoc :: (Int,Int)
               } deriving (Eq)

cLoc :: MazeLoc -> (Int,Int)
cLoc (Cell _ _ l) = l

instance Show MazeLoc where
  show (Cell _ r _) = show r
  
data Actor = Robot
             {
               _loc :: MazeLoc
             }
           | Reward
             {
               _aReward :: Double
             , _loc :: MazeLoc
             } deriving (Show, Eq)
                                  
getReward :: Maze -> (Int,Int) -> Double
getReward m@(Maze _ _ _ r) i = case r of
                                 Nothing -> _cReward $ getCell m i
                                 (Just reward) -> if _cLoc (_loc reward) == i
                                                  then _aReward reward
                                                  else 0
                    
getMoveCell :: Maze -> MazeLoc -> Move -> Maybe MazeLoc
getMoveCell maze loc move =
  let moveNum = moveToNum move
      connected = (_adj loc) !! moveNum
  in if connected
     then Just (getCell maze (getIndex (_cLoc loc) move))
     else Nothing

getCell :: Maze -> (Int, Int) -> MazeLoc
getCell m l = (_maze m) ! l

getIndex :: (Int, Int) -> Move -> (Int, Int)
getIndex (i,j) move = case move of
                        North -> (i-1,j)
                        South -> (i+1,j)
                        East  -> (i  ,j+1)
                        West  -> (i  ,j-1)

moveActor :: Actor -> Maybe MazeLoc -> Actor
moveActor a Nothing      = a
moveActor (Robot _) l    = Robot (fromJust l)
moveActor (Reward r _) l = Reward r (fromJust l)

move :: RandomGen g => Maze -> Double -> g -> Actor -> Move -> (g, Actor)
move maze p g a desiredMove =
  let (g',move) = probSelect p g desiredMove
  in (g', moveActor a (getMoveCell maze (_loc a) move))

-- with prob p, return b. Else, return a random value
probSelect :: (RandomGen g, Random b) => Double -> g -> b -> (g,b)
probSelect p gen b = let (d, g') = random gen
                     in if d < p then (g',b) else let (x,y) = random g' in (y,x)

moveToNum :: Move -> Int
moveToNum x = case x of
                North -> 0
                East  -> 1
                South -> 2
                West  -> 3

createMaze :: (Int, Int) -> [[Int]] -> [Double] -> Maze
createMaze (i,j) adjs scores = Maze start (fromList i j maze) Nothing Nothing
  where
    start = maze !! 0
    maze = map (\(x,y) -> let ind = i*(y-1) + (x-1)
                              bools = map (== 1) (adjs !! ind)
                          in Cell bools (scores !! ind) (x,y))
           [(x',y') | x' <- [1..i], y' <- [1..j]]
                          
setActor :: Maybe Actor -> Maybe Actor -> Maze -> Maze
setActor robot reward (Maze s m _ _) = Maze s m robot reward

validDirs :: [Bool] -> [Move]
validDirs (n:e:s:w:[]) = let n1 = if n then [North] else []
                             e1 = if e then East : n1 else n1
                             s1 = if s then South : e1 else e1
                         in if w then West : s1 else s1
validDirs _ = undefined

showMaze :: Maze -> Matrix String
showMaze (Maze _ m rob rew) = let mS = fmap show m
                              in showA rob $ showA rew mS

showA :: Maybe Actor -> Matrix String -> Matrix String
showA Nothing m = m
showA (Just (Robot l)) m = setElem "R" (_cLoc l) m
showA (Just (Reward r l)) m = setElem (show r) (_cLoc l) m
