module Util.Grid (Point (..), enumerateGrid, gridMap) where

import Data.Bifunctor
import qualified Data.Map as Map
import Data.Tuple
import Util.Lists

data Point = Point {getX :: Int, getY :: Int}
    deriving (Show, Eq, Ord)

gridMap :: [String] -> Map.Map Point Char
gridMap grid = Map.fromList (enumerateGrid grid)

enumerateGrid :: [String] -> [(Point, Char)]
enumerateGrid grid = concat (enumerateRows grid)

enumerateRows :: [String] -> [[(Point, Char)]]
enumerateRows grid = fmap rowToPoints cells
  where
    rows = enumerate grid
    cells = fmap (first enumerate) rows
    rowToPoints (cs, row) = fmap (first (Point row) . swap) cs
