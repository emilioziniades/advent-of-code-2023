module Day16 (energizedSquares) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Util.Grid

data Direction = Right | Down | Left | Up

energizedSquares :: FilePath -> IO Int
energizedSquares filename = do
    file <- readFile filename
    let grid = gridMap (lines file)
    print $ Map.lookup (Point 1 0) grid
    print file
    pure $ Set.size $ countVisited grid Set.empty

countVisited :: Map.Map Point Char -> Set.Set Point -> Set.Set Point
countVisited grid visited = visited
