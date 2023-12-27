{-# LANGUAGE TupleSections #-}

module Day17 (minimizeHeatLoss) where

import Data.Char (digitToInt)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.PSQueue as PSQ
import Util.Grid (Point (Point), gridMap, manhattan)
import Util.Path
import Prelude hiding (Left, Right)

data Direction = Right | Up | Left | Down
    deriving (Show, Eq, Ord)

type StraightCount = Int
type Node = (Point, Direction, StraightCount)
type Grid = Map.Map Point Int
type NeighbourFn = Node -> [Node]

-- Part 1

minimizeHeatLoss :: FilePath -> IO Int
minimizeHeatLoss filename = do
    file <- readFile filename
    pure $ findShortestPath file getNextNodesCrucible

findShortestPath :: String -> (Grid -> NeighbourFn) -> Int
findShortestPath file neighbourFn =
    aStar
        (neighbourFn grid)
        goalCheckFn
        costFn
        heuristicFn
        (PSQ.singleton startNode 0)
        (Map.singleton startNode Nothing)
        (Map.singleton startNode 0)
  where
    rawGrid = lines file
    grid = gridMap (parseGrid file)
    endPoint = Point (length rawGrid - 1) (length (head rawGrid) - 1)
    startNode = (Point 0 0, Right, 0)
    goalCheckFn = (== endPoint) . fst3
    costFn = fromJust . (`Map.lookup` grid) . fst3
    heuristicFn = manhattan endPoint . fst3

getNextNodesCrucible :: Grid -> Node -> [Node]
getNextNodesCrucible grid (Point x y, direction, straightCount) = filter ((`Map.member` grid) . fst3) nextNodes
  where
    left = (Point x (y - 1),Left,)
    right = (Point x (y + 1),Right,)
    up = (Point (x - 1) y,Up,)
    down = (Point (x + 1) y,Down,)
    nextNodes =
        if straightCount >= 2
            then case direction of
                Up -> [left 0, right 0]
                Right -> [up 0, down 0]
                Down -> [left 0, right 0]
                Left -> [up 0, down 0]
            else case direction of
                Up -> [left 0, up (straightCount + 1), right 0]
                Right -> [up 0, right (straightCount + 1), down 0]
                Down -> [left 0, down (straightCount + 1), right 0]
                Left -> [up 0, left (straightCount + 1), down 0]

-- Input parsing

parseGrid :: String -> [[Int]]
parseGrid file = fmap (fmap digitToInt) (lines file)

-- Utility

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
