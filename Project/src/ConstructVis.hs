{-# LANGUAGE DataKinds #-}

module ConstructVis
       (
         constructVisGraph
       ) where

import Data.Geometry.Polygon
import Data.Geometry.Point
import Data.Geometry.Boundary
import Data.Geometry.LineSegment
import Data.Ext
import Data.List.NonEmpty (toList)
import qualified Data.CircularSeq as C

import qualified Data.Map.Strict as Map
import qualified Data.HashSet as Set

import VertexPolygon
import PolyVis

import System.IO.Unsafe

constructVisGraph :: Double -> Double -> [Vertex] -> [Obstacle] -> VisGraph -> VisGraph
constructVisGraph r n pts obs vis = bothWayVerts
  where
    polys = map verticesToPoly obs :: [SimplePolygon () Double]
    roundPolys = map (roundCorners r n) polys
    allObsEdges = foldl (addVisDiags r n) vis polys :: VisGraph
    allEdges = foldl (addPolygon polys roundPolys) allObsEdges roundPolys :: VisGraph
    withVerts = addPoints polys roundPolys allEdges pts
    bothWayVerts = foldl insertOtherWay withVerts pts

insertOtherWay :: VisGraph -> Vertex -> VisGraph
insertOtherWay m v =
  let neighbors = m Map.! v
  in Set.foldl' (\g p -> insertEdge g (p,v)) m neighbors

addVisDiags :: Double -> Double -> VisGraph -> SimplePolygon () Double -> VisGraph
addVisDiags r n m poly =
  let circles = getCirclesAround r n poly
      roundPoly = roundCorners r n poly
      mEdges = addPolygonEdges m roundPoly
  in foldl (addPolygon [poly] circles) mEdges circles

-- A set of points centered at pt with n points
getCircleAround r n (x,y) =
  let step = 2.0*pi / n
      angles = map (\i -> step * i + 0.000001) [1..n]
      pts = map (\t -> let (dx,dy) = (r * cos t, r * sin t)
                           x' = if abs dx < 0.00001 then x else dx + x
                           y' = if abs dy < 0.00001 then y else dy + y
                       in (x',y')) angles
  in verticesToPoly pts

-- get a list of circles centered around the points in the polygon
--getCirclesAround :: _
getCirclesAround r n = map (getCircleAround r n) . map pointToVertex . polyToPoints
      
-- returns a polygon comprised of connected circles centered around
-- the points in the input polygon with edges between.
roundCorners r n poly =
  let circles = getCirclesAround r n poly
      pts = pointsOutside poly $ concatMap polyToPoints circles
  in verticesToPoly $ map pointToVertex pts


pointsOutside poly = filter (pointOutside poly)
pointOutside poly pt = case (inPolygon pt poly) of
  Outside -> True
  _      -> False
