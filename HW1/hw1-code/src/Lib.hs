module Lib
    (
      State (..)
    , runAstar
    , bresenhamBasedSuperCoverLineAlg
    ) where

import FileHandle
import MyTypes
import AStar

import System.Timeout
import Control.Exception
import System.TimeIt (timeItT)

import Data.Matrix
import Data.PSQueue
import qualified Data.Map.Strict as Map
import Data.Maybe
import Prelude hiding (lookup, null)


type FileName = String

runAstar :: FileName -> FileName -> Start -> Goal -> Epsilon -> Double -> IO ()
runAstar inputFile outputFile start goal epsilon timelimit = do
  worldMap <- fileToMap inputFile
  path     <- runAStarTimed [] worldMap start goal epsilon timelimit
  let worldWithPath = displayPathOnMap worldMap path
  mapToFile outputFile worldWithPath

bresenhamBasedSuperCoverLineAlg :: Pos -> Pos -> [Pos]
bresenhamBasedSuperCoverLineAlg p1@(x,y) p2@(x',y') =
  if p1 == p2 then [p1]
  else let dxt = x' - x
           dyt = y' - y
           (ystep,dy) = if dyt < 0 then ((-1), (-dyt)) else (1,dyt)
           (xstep,dx) = if dxt < 0 then ((-1), (-dxt)) else (1,dxt)
           ddy = 2 * dy
           ddx = 2 * dx
           firstOct = ddx >= ddy
       in if firstOct
          then bresenhamHelperFirstOct 0 dx dy ddy ddx xstep ystep dx dx p1
          else bresenhamHelperOtherOct 0 dx dy ddy ddx xstep ystep dy dy p2

bresenhamHelperFirstOct n dx dy ddy ddx xstep ystep error errorprev (i,j)
  | n >= dx = []
  | otherwise = let x = xstep + i
                    err = error + ddy
                    err2 = if err > ddx then err - ddx else err
                    y = if err > ddx then j + ystep else j
                    crossedSquares = case compare (err2 + errorprev) ddx of
                                       LT -> [(x,y),(x,y-ystep)]
                                       GT -> [(x,y),(x-xstep,y)]
                                       EQ -> [(x,y),(x,y-ystep),(x-xstep,y)]
                in crossedSquares ++ (bresenhamHelperFirstOct (n+1) dx dy ddy ddx xstep ystep err2 err2 (x,y))

bresenhamHelperOtherOct n dx dy ddy ddx xstep ystep error errorprev (i,j)
  | n >= dy = []
  | otherwise = let y              = ystep + j
                    err            = error + ddx
                    err2           = if err > ddy then err - ddy else err
                    x              = if err > ddy then i + xstep else i
                    crossedSquares = case compare (err2 + errorprev) ddy of
                                       LT -> [(x,y),(x-xstep,y)]
                                       GT -> [(x,y),(x,y-ystep)]
                                       EQ -> [(x,y),(x-xstep,y),(x,y-ystep)]
                in crossedSquares ++ (bresenhamHelperOtherOct (n+1) dx dy ddy ddx xstep ystep err2 err2 (x,y))

-- Given a time in microseconds and an action, runs and times that action
--  subjected to a timelimit. Returns the time used and a Maybe result
-- Problem: These problems are too small for this approach to work
-- Running A* takes a few microseconds. To cut off with a timelimit
--  takes several millisecons. I believe the source is that the timeout
--  function is racing two threads, and has much more overhead
-- Solution: Run code without constraining runtime, but with timing. 
timeTimed :: (Show a) => Int -> a -> IO (Int, Maybe a)
timeTimed t f = do
  (doubleTime, maybeR) <- timeItT (timeout t $ evaluate f)
  let timeInMicroseconds = doubleTime * 1e6
      integerTime        = ceiling timeInMicroseconds
  return (integerTime, maybeR)

runAStarTimed :: Path -> WorldMap -> Start -> Goal -> Epsilon -> Double -> IO Path
runAStarTimed previousPath w s g epsilon timelimit = do
  (time, (path, n)) <- timeItT (return $ aStar w s g epsilon)
  putStrLn $ "Time: " ++ (show time) ++ " Expanded: " ++ (show n) ++ " Path length: " ++ (show $ length path) ++ " Epsilon: " ++ show epsilon
  let newE = epsilon - 0.5 * (epsilon - 1)
  if newE == 1.0
    then return path
    else let newEps = if newE < 1.001 then 1.0 else newE in runAStarTimed path w s g newEps timelimit
  
