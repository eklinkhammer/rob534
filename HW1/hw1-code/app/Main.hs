{-# OPTIONS -Wall #-}
module Main where

import Lib

timelimit :: Double
timelimit = 0.05

epsilon :: Double
epsilon = 10

main :: IO ()
main = do
  let inputFile  = "maze2.pgm"
      outputFile = "maze2_solved.pgm"
      start = TwoD (1,1) --(0,0)
      goal = TwoD (25,25)-- (0,0)
  runAstar inputFile outputFile start goal epsilon timelimit
