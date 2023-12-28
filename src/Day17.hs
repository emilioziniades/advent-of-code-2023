{-# LANGUAGE TupleSections #-}

module Day17 (minimizeHeatLoss, minimizeHeatLossUltra) where

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
    pure $ findShortestPath file getNextNodesCrucible isNodeAtEnd

getNextNodesCrucible :: Grid -> Node -> [Node]
getNextNodesCrucible grid (Point x y, direction, straightCount)
    | straightCount >= 3 = filterOOBNodes $ goTurn direction
    | otherwise = filterOOBNodes $ goStraight direction : goTurn direction
  where
    left = (Point x (y - 1),Left,)
    right = (Point x (y + 1),Right,)
    up = (Point (x - 1) y,Up,)
    down = (Point (x + 1) y,Down,)
    goTurn Up = [left 1, right 1]
    goTurn Right = [up 1, down 1]
    goTurn Down = [left 1, right 1]
    goTurn Left = [up 1, down 1]
    goStraight Up = up (straightCount + 1)
    goStraight Right = right (straightCount + 1)
    goStraight Down = down (straightCount + 1)
    goStraight Left = left (straightCount + 1)
    filterOOBNodes = filter ((`Map.member` grid) . fst3)

isNodeAtEnd :: Point -> Node -> Bool
isNodeAtEnd endPoint (point, _, _) = endPoint == point

-- Part 2

minimizeHeatLossUltra :: FilePath -> IO Int
minimizeHeatLossUltra filename = do
    file <- readFile filename
    pure $ findShortestPath file getNextNodesUltraCrucible isNodeAtEndAndStoppable

getNextNodesUltraCrucible :: Grid -> Node -> [Node]
getNextNodesUltraCrucible grid (Point x y, direction, straightCount)
    | straightCount < 4 = filterOOBNodes $ pure $ goStraight direction
    | straightCount >= 10 = filterOOBNodes $ goTurn direction
    | otherwise = filterOOBNodes $ goStraight direction : goTurn direction
  where
    left = (Point x (y - 1),Left,)
    right = (Point x (y + 1),Right,)
    up = (Point (x - 1) y,Up,)
    down = (Point (x + 1) y,Down,)
    goTurn Up = [left 1, right 1]
    goTurn Right = [up 1, down 1]
    goTurn Down = [left 1, right 1]
    goTurn Left = [up 1, down 1]
    goStraight Up = up (straightCount + 1)
    goStraight Right = right (straightCount + 1)
    goStraight Down = down (straightCount + 1)
    goStraight Left = left (straightCount + 1)
    filterOOBNodes = filter ((`Map.member` grid) . fst3)

isNodeAtEndAndStoppable :: Point -> Node -> Bool
isNodeAtEndAndStoppable endPoint (point, _, straightCount) = endPoint == point && straightCount >= 4

-- Common to Part 1 and 2

findShortestPath :: String -> (Grid -> NeighbourFn) -> (Point -> Node -> Bool) -> Int
findShortestPath file neighbourFn goalCheckFn =
    aStar
        (neighbourFn grid)
        (goalCheckFn endPoint)
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
    costFn = fromJust . (`Map.lookup` grid) . fst3
    heuristicFn = manhattan endPoint . fst3

-- Input parsing

parseGrid :: String -> [[Int]]
parseGrid file = fmap (fmap digitToInt) (lines file)

-- Utility

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
