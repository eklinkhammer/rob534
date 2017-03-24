{-# LANGUAGE DataKinds #-}

module VertexPolygonTest
       (
         vertexPolygonTests
       ) where
      
import Test.HUnit

import VertexPolygon
import System.IO.Unsafe

square = [(0,0), (1,0), (1,1), (0,1)] :: Obstacle

vertexPolygonTests = TestList [ TestLabel "test1" test1
                              , TestLabel "test2" testVertToVert
                              , TestLabel "test3" testEdgesBasic
                              , TestLabel "test4" testX]

test1 = TestCase (do
                     let x = 5
                     assertEqual "test1" 5 5)

testVertToVert = TestCase (do
                              let poly = verticesToPoly square
                                  pts  = polyToPoints poly
                                  vts  = map pointToVertex pts
                              assertEqual "test" square vts)

testEdgesBasic = TestCase (do
                              let poly  = verticesToPoly square
                                  edges = polyToEdges poly
                                  expected =
                                    [
                                      ((0,0),(1,0))
                                    , ((1,0),(1,1))
                                    , ((0,1),(1,1))
                                    , ((0,0),(0,1))
                                    ]
                              assertEqual "test" expected edges)
                                    
testX = undefined
