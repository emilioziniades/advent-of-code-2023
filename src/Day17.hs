{-# LANGUAGE TupleSections #-}

module Day17 (minimizeHeatLoss) where

import Data.Char (digitToInt)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.PSQueue as PSQ
import Debug.Trace (trace)
import Util.Grid (Point (Point), gridMap)
import Prelude hiding (Left, Right)

data Direction = Right | Up | Left | Down
    deriving (Show, Eq, Ord)

minimizeHeatLoss :: FilePath -> IO Int
minimizeHeatLoss filename = do
    file <- readFile filename
    let grid = gridMap $ parseGrid file
    let rawGrid = lines file
    let goal = Point (length rawGrid - 1) (length (head rawGrid) - 1)
    let startNode = (Point 0 0, Right, 0)
    let costs = breadthFirst grid goal (PSQ.singleton startNode 0) Map.empty (Map.singleton startNode 0)
    print $ Map.filterWithKey (\k _ -> fst3 k == goal) costs
    pure 0

parseGrid :: String -> [[Int]]
parseGrid file = fmap (fmap digitToInt) (lines file)

type StraightCount = Int
type Node = (Point, Direction, StraightCount)

breadthFirst :: Map.Map Point Int -> Point -> PSQ.PSQ Node Int -> Map.Map Node Node -> Map.Map Node Int -> Map.Map Node Int
breadthFirst grid goal queue cameFrom costSoFar
    | PSQ.null queue = trace "queue is depleted" costSoFar
    | fst3 (PSQ.key current) == goal = trace "found goal" costSoFar
    | otherwise = breadthFirst grid goal newQueue newCameFrom newCostSoFar
  where
    (current, restQueue) = fromJust $ PSQ.minView queue
    currentNode = PSQ.key current
    nextNodes = filter (`Map.notMember` cameFrom) $ filter (flip Map.member grid . fst3) $ getNextNodes currentNode
    nextCosts = fmap (getNewCost grid costSoFar currentNode) nextNodes
    newQueue = foldl (\q (n, p) -> PSQ.insert n p q) restQueue nextCosts
    newCostSoFar = foldr (\(n, c) cs -> Map.insert n c cs) costSoFar nextCosts
    newCameFrom = foldr (`Map.insert` currentNode) cameFrom nextNodes

getNewCost :: Map.Map Point Int -> Map.Map Node Int -> Node -> Node -> (Node, Int)
getNewCost grid costSoFar current next = (next, currentCost + nextCost)
  where
    currentCost = fromJust $ Map.lookup current costSoFar
    nextCost = fromJust $ Map.lookup (fst3 next) grid

getNextNodes :: Node -> [Node]
getNextNodes (Point x y, direction, straightCount)
    | straightCount >= 3 = case direction of
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

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
