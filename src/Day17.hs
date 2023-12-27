{-# LANGUAGE TupleSections #-}

module Day17 (minimizeHeatLoss) where

import Data.Char (digitToInt)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.PSQueue as PSQ
import Util.Grid (Point (Point), gridMap)
import Prelude hiding (Left, Right)

data Direction = Right | Up | Left | Down
    deriving (Show, Eq, Ord)

type StraightCount = Int
type Node = (Point, Direction, StraightCount)
type Frontier = PSQ.PSQ Node Int
type CameFrom = Map.Map Node (Maybe Node)
type CostSoFar = Map.Map Node Int
type Goal = Point
type Grid = Map.Map Point Int

-- Part 1

minimizeHeatLoss :: FilePath -> IO Int
minimizeHeatLoss filename = do
    file <- readFile filename
    let (n, _) = findShortestPath file
    pure n

findShortestPath :: String -> (Int, CameFrom)
findShortestPath file = runAStar grid startNode endPoint
  where
    rawGrid = lines file
    grid = gridMap (parseGrid file)
    endPoint = Point (length rawGrid - 1) (length (head rawGrid) - 1)
    startNode = (Point 0 0, Right, 0)

runAStar :: Grid -> Node -> Goal -> (Int, CameFrom)
runAStar grid startNode endPoint =
    aStar
        grid
        endPoint
        (PSQ.singleton startNode 0)
        (Map.singleton startNode Nothing)
        (Map.singleton startNode 0)

aStar :: Grid -> Goal -> Frontier -> CameFrom -> CostSoFar -> (Int, CameFrom)
aStar grid endPoint queue cameFrom costSoFar
    | PSQ.null queue = error "queue exhausted before target reached"
    | fst3 currentNode == endPoint = (fromJust $ Map.lookup currentNode costSoFar, cameFrom)
    | otherwise = aStar grid endPoint newQueue newCameFrom newCostSoFar
  where
    (current, restQueue) = fromJust (PSQ.minView queue)
    currentNode = PSQ.key current
    nextNodes = filter ((`Map.member` grid) . fst3) $ getNextNodes currentNode
    (newQueue, newCameFrom, newCostSoFar) = foldr (updateMaps grid currentNode) (restQueue, cameFrom, costSoFar) nextNodes

updateMaps :: Grid -> Node -> Node -> (Frontier, CameFrom, CostSoFar) -> (Frontier, CameFrom, CostSoFar)
updateMaps grid current next (frontier, cameFrom, costSoFar)
    | Map.notMember next costSoFar || newCost < existingNewCost = (newFrontier, newCameFrom, newCostSoFar)
    | otherwise = (frontier, cameFrom, costSoFar)
  where
    nextPoint = fst3 next
    currentCost = fromJust $ Map.lookup current costSoFar
    nextCost = fromJust $ Map.lookup nextPoint grid
    newCost = currentCost + nextCost
    existingNewCost = fromJust $ Map.lookup next costSoFar
    newFrontier = PSQ.insert next newCost frontier
    newCameFrom = Map.insert next (Just current) cameFrom
    newCostSoFar = Map.insert next newCost costSoFar

getNextNodes :: Node -> [Node]
getNextNodes (Point x y, direction, straightCount)
    | straightCount >= 2 = case direction of
        Up -> [left 0, right 0]
        Right -> [up 0, down 0]
        Down -> [left 0, right 0]
        Left -> [up 0, down 0]
    | otherwise = case direction of
        Up -> [left 0, up (straightCount + 1), right 0]
        Right -> [up 0, right (straightCount + 1), down 0]
        Down -> [left 0, down (straightCount + 1), right 0]
        Left -> [up 0, left (straightCount + 1), down 0]
  where
    left = (Point x (y - 1),Left,)
    right = (Point x (y + 1),Right,)
    up = (Point (x - 1) y,Up,)
    down = (Point (x + 1) y,Down,)

-- Input parsing

parseGrid :: String -> [[Int]]
parseGrid file = fmap (fmap digitToInt) (lines file)

-- Utility

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
