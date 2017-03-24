{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module VertexPolygon
       (
         verticesToPoly
       , polyToPoints
       , polyToEdges
       , polyToPtsEdges
       , polysToPtsEdges
       , pointToVertex
       , polyToOuterEdges
       , Vertex
       , Edge
       , Obstacle
       ) where

import Data.Geometry.Polygon
import Data.Geometry.Point
import Data.Geometry.Boundary
import Data.Geometry.LineSegment
import Data.Ext
import Data.List.NonEmpty (toList)
import qualified Data.CircularSeq as C
import Algorithms.Geometry.DelaunayTriangulation.DivideAndConqueror
import Algorithms.Geometry.DelaunayTriangulation.Types hiding (Vertex)
import qualified Data.Vector as V
import qualified Data.HashSet as S
import System.IO.Unsafe
import Data.List

type Vertex   = (Double, Double)
type Edge     = (Vertex, Vertex)
type Obstacle = [Vertex]

verticesToPoly = SimplePolygon . C.fromList . map (ext . vertexToPoint)

polyToPoints = map _core . toList . polygonVertices
polyToEdges = map segmentToEdge . toList . C.toNonEmpty . outerBoundaryEdges

polyToOuterEdges = polyToEdges . naiveConcaveHull

naiveConcaveHull poly =
  let edges = triangulationEdges $ triangulation poly
      edgeVertex = unsafePerformIO $ do
        let x = map arrangeEdge edges
        putStrLn $ "\nAll edges: " ++ (show x)
        return x
      uniqueEdges = unsafePerformIO $ do
        let x = S.toList $ S.fromList edgeVertex
        putStrLn $ "\nUnique Edges: " ++ (show x)
        return x
      pts = unsafePerformIO $ do
        let x = concatMap (\(a,b) -> a : [b]) uniqueEdges
        putStrLn $ "\nPts: " ++ (show x)
        return x
      uniquePts = unsafePerformIO $ do
        let x = S.toList $ S.fromList pts
            y = map (\z -> (length z, head z)) $ group $ sort pts
            us = map snd $ takeWhile (\a -> fst a < 4) $ sortOn fst y
        putStrLn $ "\nUnique pts: " ++ (show y)
        return x
  in verticesToPoly uniquePts

triangulation = delaunayTriangulation . polygonVertices

arrangeEdge (x,y) =
  let (a,b) = (pointToVertex $ _core x, pointToVertex $ _core y)
  in if a > b then (b,a) else (a,b)
      

segmentToEdge seg = let (p1,p2) = orderedEndPoints seg
                    in (pointToVertex $ _core p1, pointToVertex $ _core p2)

vertexToPoint (a,b) = point2 a b

pointToVertex :: Point 2 Double -> Vertex
pointToVertex = _point2

polyToPtsEdges poly = (polyToPoints poly, polyToEdges poly)

polysToPtsEdges polys = let pts = concatMap polyToPoints polys
                            edges = concatMap polyToEdges polys
                        in (map pointToVertex pts, edges)
