module Util.Grid (Point (..), enumerateGrid, gridMap, manhattan) where

import Data.Bifunctor
import qualified Data.Map as Map
import Data.Tuple
import Util.Lists

data Point = Point {getX :: Int, getY :: Int}
    deriving (Show, Eq, Ord)

gridMap :: [[a]] -> Map.Map Point a
gridMap grid = Map.fromList (enumerateGrid grid)

enumerateGrid :: [[a]] -> [(Point, a)]
enumerateGrid grid = concat (enumerateRows grid)

enumerateRows :: [[a]] -> [[(Point, a)]]
enumerateRows grid = fmap rowToPoints cells
  where
    rows = enumerate grid
    cells = fmap (first enumerate) rows
    rowToPoints (cs, row) = fmap (first (Point row) . swap) cs

manhattan :: Point -> Point -> Int
manhattan (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)
