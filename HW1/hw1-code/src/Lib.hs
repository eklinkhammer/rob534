module Lib
    ( someFunc
    ) where

import Display
import MyTypes
import AStar

import Data.Matrix
import Data.PSQueue
import qualified Data.Map.Strict as Map
import Data.Maybe
import Prelude hiding (lookup, null)


someFunc :: IO ()
someFunc = do
  let worldMap = Data.Matrix.fromList 3 3 [0,0,1,0,0,0,0,0,0]
      path     = aStar worldMap (1,1) (3,3)
  showMapWithPath worldMap path
