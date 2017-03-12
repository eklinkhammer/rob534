module MDP
  (
    train
  , qNavigate
  , trainPMDP
  , mostLikely
  , qMDPNavigate
  , updateBeliefNotFound
  ) where

import Data.Matrix
import Data.Maybe
import Data.List
import System.Random

import Numeric

import Maze
import System.IO.Unsafe

type ValuePolicy = Matrix Double
type DiscountFactor = Double
type Noise = Double

iter :: Noise -> DiscountFactor -> Maze -> ValuePolicy -> ValuePolicy
iter p gamma maze v = fmap (getValue p gamma maze v) (_maze maze)

getValue :: Noise -> DiscountFactor -> Maze -> ValuePolicy -> MazeLoc -> Double
getValue p gamma maze policy c =
  let actionScores = getActionScores p gamma maze policy c
  in if null actionScores then 0 else maximum actionScores

getActionScores :: Noise -> DiscountFactor -> Maze -> ValuePolicy -> MazeLoc -> [Double]
getActionScores p gamma maze policy c@(Cell adj r l) = actionScores
  where
    ms  = validDirs adj
    ss' = map (fromJust . getMoveCell maze c) ms --set of reachable states
    actionScores = map (\a -> actionValue p gamma maze policy c (delete a ms) a) ms
    
actionValue :: Noise -> DiscountFactor -> Maze -> ValuePolicy -> MazeLoc -> [Move] -> Move -> Double
actionValue p g m v s otherMoves move =
  let r = getReward m (_cLoc s)
      pOther = p / 4
      -- pTarget includes prob of error going the right direction
      -- pStay is the prob you err in the direction of an obstacle
      pStay = pOther * (3 - (fromIntegral $ length otherMoves))
      pTarget = (1-p) + pOther
      others = map (\x -> let value = v ! (_cLoc (fromJust (getMoveCell m s x)))
                          in pOther * (r + g * value)) otherMoves
      intended = let value = v ! (_cLoc (fromJust (getMoveCell m s move)))
                 in (pTarget) * (r + g * value)
      stay = let value = v ! (_cLoc s)
             in pStay * (r + g * value)
  in sum others + intended + stay
  
rep :: Int -> (a -> a) -> a -> a
rep 0 _ a = a
rep n f a = rep (n - 1) f (f a)

train :: Int -> Noise -> DiscountFactor -> Maze -> ValuePolicy -> ValuePolicy
train n p gamma maze init = rep n (iter p gamma maze) init

qNavigate :: RandomGen g
  => Int -> Noise -> DiscountFactor -> Maze -> ValuePolicy -> g -> (g, Double)
qNavigate 0 _ _ _ _ g = (g,0)
qNavigate n p gam maze vals g =
  let robot = _robot maze
  in case robot of
       Nothing -> (g,0)
       (Just (Robot l)) -> let (gf, horizon) = qNavigate (n-1) p gam maze' vals g'
                           in (gf, gam * horizon + (getReward maze (_cLoc l)))
         where
           maze' = setActor (Just robot') reward' maze
           (g', robot') = move maze (1-p) g (Robot l) desiredMove
           movesScores = zip (getActionScores p gam maze vals l) (validDirs (_adj l))
           desiredMove = snd $ last $ sortOn fst movesScores
           (g1, reward') = case (_movingReward maze) of
                             Nothing -> (g', Nothing)
                             (Just r) -> (\(a,b) -> (a, Just b)) $ move maze 0 g' r North
  



-- before, value policy was a mapping from location to value
-- now, value policy is a mapping from belief and location to value
type QValuePolicy = Matrix ValuePolicy


trainPMDP :: Int -> Noise -> DiscountFactor -> Maze -> QValuePolicy -> QValuePolicy
trainPMDP n p g m init = rep n (iterPMDP p g m) init

iterPMDP :: Noise -> DiscountFactor -> Maze -> QValuePolicy -> QValuePolicy
iterPMDP p gamma maze qv = fmap (getValuePolicy p gamma maze qv) (_maze maze)

-- Given QV, get next QV_s where s is the state
getValuePolicy :: Noise -> DiscountFactor -> Maze -> QValuePolicy -> MazeLoc -> ValuePolicy
getValuePolicy n g m qv i = fmap (getValuePMDP n g m qv i) (_maze m) 

getValuePMDP :: Noise -> DiscountFactor -> Maze -> QValuePolicy -> MazeLoc -> MazeLoc -> Double
getValuePMDP n g m qv qi i = let scores = getActionScoresPMDP n g m qv qi i
                             in if null scores then 0 else maximum scores
-- Gets the scores 
getActionScoresPMDP :: Noise -> DiscountFactor -> Maze -> QValuePolicy -> MazeLoc -> MazeLoc -> [Double]
getActionScoresPMDP n g m qv iv i = actionScores
  where
    validMoves = validDirs (_adj i)
    states     = map (fromJust . getMoveCell m i) validMoves
    actionScores = map (\a -> actionValuePMDP n g m qv iv i (delete a validMoves) a) validMoves

-- 
actionValuePMDP :: Noise -> DiscountFactor -> Maze -> QValuePolicy -> MazeLoc -> MazeLoc -> [Move] -> Move -> Double
actionValuePMDP p g m v qi i moves move =
  let r = getReward m (_cLoc i)
      pOther = p / 4
      pStay = pOther * (3 - (fromIntegral $ length moves))
      pTarget = (1-p) + pOther

      others = map (\x -> let eValue = expectedValue v qi (fromJust (getMoveCell m i x))
                          in pOther * (r + g * eValue)) moves
      stay = let eValue = expectedValue v qi i
             in pStay * (r + g * eValue)
      intended = let eValue = expectedValue v qi (fromJust (getMoveCell m i move))
                 in (pTarget * (r + g * eValue))
  in sum others + stay + intended


-- Given that the reward is currently in a location and the robot is moving to a new location,
-- what is the expected value of the robot given that the reward can move
expectedValue :: QValuePolicy -> MazeLoc -> MazeLoc -> Double
expectedValue qv reward robot =
  let validRewardMoves = validDirs (_adj reward)
      validRewardPolicies = valuePolicies qv reward validRewardMoves
      pStay = (4.0 - (fromIntegral $ length validRewardMoves)) / 4
      pMove = 0.25
      movingScores = map (\policy -> policy ! (_cLoc robot)) validRewardPolicies
      stayScore = (qv ! (_cLoc reward)) ! (_cLoc robot)
  in (pStay * stayScore) + (pMove * sum movingScores)
  
valuePolicies :: QValuePolicy -> MazeLoc -> [Move] -> [ValuePolicy]
valuePolicies qv loc ms = let i = getIndex (_cLoc loc)
                          in map (\m -> qv ! (i m)) ms

              
type Belief = Matrix Double

-- Updates belief once reward has been found
updateBeliefFound :: Belief -> (Int, Int) -> Belief
updateBeliefFound b i = setElem 1.0 i (fmap (\_ -> 0.0) b)

-- Updates belief one actor has moved into a cell and not discovered a reward
updateBeliefNotFound :: Belief -> Maze -> Belief
updateBeliefNotFound b maze =
  let robLoc = _loc $ fromJust $ _robot maze
      knowNotHere = setElem 0 (_cLoc robLoc) b
  in fmap (updateBeliefI knowNotHere maze) (_maze maze)

updateBeliefI :: Belief -> Maze -> MazeLoc -> Double
updateBeliefI b m loc =
  let ms = validDirs (_adj loc)
      ss = map (_cLoc . fromJust . getMoveCell m loc) ms
      val = sum $ map (\s -> b ! s) ss
      valCome = 0.25 * (fromIntegral $ length ss) * val
      valStay = (1 - 0.25 * (fromIntegral $ length ss)) * (b ! (_cLoc loc))
  in valStay + valCome

qMDPNavigate :: RandomGen g
  => Int -> Noise -> DiscountFactor -> Maze -> QValuePolicy -> Belief -> g -> (g, Double)
qMDPNavigate 0 _ _ _ _ _ g = (g,0)
qMDPNavigate n p g maze qv b gen =
  let robot = fromJust $ _robot maze
      reward = fromJust $ _movingReward maze
      (gf,horizon) = qMDPNavigate (n-1) p g maze' qv b' g2
      maze' = setActor (Just robot') (Just reward') maze
      l = _loc robot
      (g1, robot') = move maze (1-p) gen robot desiredMove
      as = validDirs (_adj l)
      policyProb = zip (toList qv) (toList b)
      actionScores = map (\a -> let vals = map (\(policy, prob) -> let val = actionValue p g maze policy l (delete a as) a
                                                                   in prob * val) policyProb
                                in (a, sum vals)) as
      desiredMove = fst $ last $ sortOn snd actionScores
      (g2, reward') = move maze 0 g1 reward North
      b' = updateBelief b maze'
  in (gf, g * horizon + (getReward maze (_cLoc l)))
    -- unsafePerformIO $ do
    -- let policies = map (\(policy, prob) -> fmap (\x -> prob * x) policy) policyProb
    --     qtable = foldl (elementwise (+)) (fromList 4 6 $ replicate 24 0) policies
    --     norm = fmap (\x -> x / 24) qtable
    -- if n < 100 then putStrLn $ prettyPrint norm else putStr ""
    -- return $

-- use the ValuePolicy corresponding ot the most likely location of the reward to pick
-- the move. Update the Maze with the move and the random move.
mostLikely :: RandomGen g
  => Int -> Noise -> DiscountFactor -> Maze -> QValuePolicy -> Belief -> g -> (g, Double)
mostLikely 0 _ _ _ _ _ g = (g,0)
mostLikely n p g maze qv b gen =
  let indexedBelief = zip [(x,y) | x <- [1..(nrows qv)], y <- [1..(ncols qv)]] (toList b)
      bestBeliefI = fst $ last $ sortOn snd indexedBelief
      policy = qv ! bestBeliefI
      robot = fromJust $ _robot maze
      reward = fromJust $ _movingReward maze
      (gf,horizon) = mostLikely (n-1) p g maze' qv b' g2
      maze' = setActor (Just robot') (Just reward') maze
      l = _loc robot
      (g1, robot') = move maze (1-p) gen robot desiredMove
      movesScores = zip (getActionScores p g maze policy l) (validDirs (_adj l))
      desiredMove = snd $ last $ sortOn fst movesScores
      (g2, reward') = move maze 0 g1 reward North
      b' = updateBelief b maze'
  in (gf, g * horizon + (getReward maze (_cLoc l)))
  -- unsafePerformIO $ do
  --   if n < 5
  --     then putStrLn $ prettyPrint policy
  --     else putStr ""
  --   return $ 

updateBelief :: Belief -> Maze -> Belief
updateBelief belief maze =
  let robLoc = _loc $ fromJust $ _robot maze
      rewardLoc = _loc $ fromJust $ _movingReward maze
  in if rewardLoc == robLoc
     then updateBeliefFound belief (_cLoc robLoc)
     else updateBeliefNotFound belief maze

prettyPrint :: Matrix Double -> String
prettyPrint = show . fmap (\v -> formatFloatN v 3)

formatFloatN floatNum numDecimals = showFFloat (Just numDecimals) floatNum ""
