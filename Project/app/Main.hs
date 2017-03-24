{-# LANGUAGE DataKinds #-}

module Main where

import VisPath

main :: IO ()
main = do
  let square = [(0,0), (1,0), (1,1), (0,1)]
      start = ((-1),0)
      end = (1.5, 0.5)
      r = 0.1
      n = 8
      path = visPath r n start end [square]
  putStrLn $ show path
