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

data State             = TwoD Pos
                       | FourD Pos Vel
                       deriving (Show)

instance Eq State where
  (==) (TwoD pos1)       (TwoD pos2)       = pos1 == pos2
  (==) (FourD pos1 vel1) (FourD pos2 vel2) = (pos1 == pos2) && (vel1 == vel2)

instance Ord State where
  (<=) (TwoD p1)     (TwoD p2)     = p1 <= p2
  (<=) (FourD p1 v1) (FourD p2 v2) = if p1 == p2 then v1 <= v2 else p1 <= p2

type NumNodesExpanded  = Int
type Pos               = (Int, Int)
type Vel               = (Int, Int)
type DistanceToGoal    = Double
type DistanceFromStart = Double
type OpenSet           = PSQ State DistanceToGoal
type ClosedSet         = Map.Map State Bool
type Previous          = Map.Map State State
type GScore            = Map.Map State DistanceFromStart
type Path              = [State]
type Start             = State
type Goal              = State
type Epsilon           = Double
infinity               = 1.0/0.0
data AStar             = AStar OpenSet ClosedSet Previous GScore


getPos :: State -> Pos
getPos (TwoD pos) = pos
getPos (FourD pos _) = pos

posToState2D :: Pos -> State
posToState2D pos = TwoD pos

posToState4D :: Pos -> State
posToState4D pos = FourD pos (0,0)
