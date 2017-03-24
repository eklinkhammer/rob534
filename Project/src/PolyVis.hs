{-# LANGUAGE DataKinds #-}

module PolyVis
       (
         VisGraph
       , addPolygon
       , addPolygonEdges
       , intersectL
       , addPoints
       , insertEdge
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
import Data.List

import VertexPolygon

type VisGraph = Map.Map Vertex (Set.HashSet Vertex)

addPolygonEdges m = foldl insertEdge m . polyToEdges

insertEdge :: VisGraph -> Edge -> VisGraph
insertEdge vis (a,b) = case (Map.lookup a vis) of
  Nothing    -> Map.insert a (Set.singleton b) vis
  (Just set) -> Map.insert a (Set.insert b set) vis

-- From StackOverFlow - intersection of two lines
intersectL (a,b) (c,d) = if a == c || a == d || b == c || b == d then False
                         else ccw a c d /= ccw b c d && ccw a b c /= ccw a b d
  where ccw (mx, my) (nx, ny) (ox, oy) = (oy - my) * (nx - mx) > (ny - my) * (ox - mx)

-- are points x and y visible to each other
-- if the line segment between them does not intersect any shape
-- already accounted for if they are the same shape
isVisible x y = foldl (&&) True . concatMap (map (not . intersectL (x,y)) . polyToEdges)


addPoints bound polys = foldl (addPoint bound polys)

addPolygon bound polys m poly =
  let noP = delete poly polys
      pts = map pointToVertex $ polyToPoints poly
  in addPoints bound noP m pts

addPoint :: [SimplePolygon a Double] -> [SimplePolygon a Double] -> VisGraph -> Vertex -> VisGraph
addPoint bound polys m pt = let pts = fst (polysToPtsEdges polys)
                            in foldl (insertIfVisible pt (bound ++polys)) m pts

insertIfVisible a ps v b = if mapSetContains v a b then v
                           else if mapSetContains v b a
                                then insertEdge v (a,b)
                                else if isVisible a b ps
                                     then insertEdge v (a,b)
                                     else v




                                       
mapSetContains m a b = case (Map.lookup a m) of
  Nothing -> False
  (Just setA) -> Set.member b setA
