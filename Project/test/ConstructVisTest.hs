{-# LANGUAGE DataKinds #-}

module ConstructVisTest
       (
         constructVisTests
       ) where
      
import Test.HUnit

import ConstructVis
import VertexPolygon

import qualified Data.Map.Strict as Map
import System.IO.Unsafe

square = [(0,0), (1,0), (1,1), (0,1)] :: Obstacle

pointShape = [(0,0)] :: Obstacle

constructVisTests = TestList [TestLabel "pointTests" pointTests]

pointTests = TestList [TestLabel "test1" edgesVisible]

edgesVisible = TestCase (do
                            let graph = unsafePerformIO $ do
                                  let x = constructVisGraph 0.1 1 [] [square] Map.empty
                                  putStrLn $ show x
                                  return x
                            
                            assertEqual "edges" Map.empty  graph)
