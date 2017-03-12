module Lib
    ( someFunc
    ) where

import Maze
import MDP
import Data.Matrix
import Numeric
import System.Random
import Data.List

adjs :: [[Int]]
adjs = [[0, 1, 1, 0]
       ,[1, 0, 1, 0]
       ,[1, 1, 1, 0]
       ,[1, 1, 0, 0]
       ,[0, 1, 0, 1]
       ,[0, 0, 0, 0]
       ,[0, 1, 0, 1]
       ,[0, 1, 0, 1]
       ,[0, 1, 1, 1]
       ,[1, 1, 1, 0]
       ,[1, 1, 0, 1]
       ,[0, 1, 0, 1]
       ,[0, 1, 1, 1]
       ,[1, 0, 1, 1]
       ,[1, 0, 0, 1]
       ,[0, 1, 0, 1]
       ,[0, 1, 1, 1]
       ,[1, 1, 1, 0]
       ,[1, 1, 1, 0]
       ,[1, 1, 0, 1]
       ,[0, 0, 1, 1]
       ,[1, 0, 1, 1]
       ,[1, 0, 1, 1]
       ,[1, 0, 0, 1]]

scores0 :: [Double]
scores0 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (-10), 1, 0, 0]

-- The score of a moving reward is represente by a a reward actor
scores1 :: [Double]
scores1 =  replicate 24 0.0

size :: (Int,Int)
size = (4,6)

formatFloatN floatNum numDecimals = showFFloat (Just numDecimals) floatNum ""

prettyPrint :: Matrix Double -> String
prettyPrint = show . fmap (\v -> formatFloatN v 3)

mean :: [Double] -> Double
mean xs = sum xs / (fromIntegral $ length xs)

someFunc :: IO ()
someFunc = do
  gen <- newStdGen
  -- Part 1
  let maze = createMaze size adjs scores0
      robot = Robot (getCell maze (1,1))
      maze1 = setActor (Just robot) Nothing maze
      initV = fromList 4 6 (replicate 24 0)
      finalV = train 1000 0.1 0.9 maze1 initV
      finalV' = train 1000 0.2 0.9 maze1 initV
      (_, scores) = mapAccumL (\g _ -> qNavigate 1000 0.1 0.9 maze1 finalV g) gen [1..10000]
      (_, scores') = mapAccumL (\g _ -> qNavigate 1000 0.2 0.9 maze1 finalV' g) gen [1..10000]

      -- Part 2
      maze' = createMaze size adjs scores1
      robot2 = Robot (getCell maze' (1,1))
      reward = Reward 1.0 (getCell maze' (4,6))
      maze2 = setActor (Just robot2) (Just reward) maze'
      initialBelief = fromList 4 6 scores1
      initQV = fromList 4 6 (replicate 24 initV)
      finalQV = trainPMDP 100 0.0 0.9 maze2 initQV
      finalQV' = trainPMDP 100 0.3 0.9 maze2 initQV
      (_, scorePMDPQ) = qMDPNavigate 100 0.0 0.9 maze2 finalQV initialBelief gen
      (_, scorePMDPML) = mostLikely 100 0.0 0.9 maze2 finalQV initialBelief gen
      (_, scorePMDPQ') = qMDPNavigate 100 0.3 0.9 maze2 finalQV' initialBelief gen
      (_, scorePMDPML') = mostLikely 100 0.3 0.9 maze2 finalQV' initialBelief gen

      (_, scoresPMDPQ) = mapAccumL (\g _ -> qMDPNavigate 100 0.0 0.9 maze2 finalQV initialBelief g) gen [1..10000]
      (_, scoresPMDPML) = mapAccumL (\g _ -> mostLikely 100 0.0 0.9 maze2 finalQV initialBelief g) gen [1..10000]
      (_, scoresPMDPQ') = mapAccumL (\g _ -> qMDPNavigate 100 0.3 0.9 maze2 finalQV' initialBelief g) gen [1..10000]
      (_, scoresPMDPML') = mapAccumL (\g _ -> mostLikely 100 0.3 0.9 maze2 finalQV' initialBelief g) gen [1..10000]


      
  putStrLn "MDP Value Tables"
  putStrLn $ prettyPrint finalV
  putStrLn $ prettyPrint finalV'

  putStrLn "MDP Scores"
  --putStrLn $ show $ mean scores
  --putStrLn $ show $ mean scores'

  putStrLn "PMDP Scores"
  putStrLn $ show $ mean scoresPMDPQ
  putStrLn $ show $ mean scoresPMDPML
  putStrLn $ show $ mean scoresPMDPQ'
  putStrLn $ show $ mean scoresPMDPML'
