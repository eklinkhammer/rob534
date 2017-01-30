module MyTypes where


import Data.Matrix
import Data.PSQueue
import qualified Data.Map.Strict as Map

-- ************************************************
-- Types, Constants, and Data Constructors
-- ************************************************
-- The map is represented as a matrix of ints, with 1 representing an
--  obstacle and 0 representing free space
type WorldMap          = Matrix Int
type Pos               = (Int, Int)
type DistanceToGoal    = Double
type DistanceFromStart = Double
type OpenSet           = PSQ Pos DistanceToGoal
type ClosedSet         = Map.Map Pos Bool
type Previous          = Map.Map Pos Pos
type GScore            = Map.Map Pos DistanceFromStart
type Path              = [Pos]
type Start             = Pos
type Goal              = Pos
infinity               = 1.0/0.0
data AStar             = AStar OpenSet ClosedSet Previous GScore