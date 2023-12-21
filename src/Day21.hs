module Day21 (countSteps, countMoreSteps) where

import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Util.Grid

type Garden = Map.Map Point Char

countSteps :: Int -> FilePath -> IO Int
countSteps n filename = do
    file <- readFile filename
    let garden = gridMap (lines file)
    pure $ countNSteps garden n

countNSteps :: Garden -> Int -> Int
countNSteps garden n = Set.size $ iterate (nextSteps garden) (Set.singleton start) !! n
  where
    start = findStart garden

nextSteps :: Garden -> Set.Set Point -> Set.Set Point
nextSteps garden visited = Set.fromList $ concatMap (neighbours garden) (Set.toList visited)

findStart :: Garden -> Point
findStart garden = fst . head . Map.toList $ Map.filter (== 'S') garden

neighbours :: Garden -> Point -> [Point]
neighbours garden (Point x y) = filter ((/= '#') . fromJust . (`Map.lookup` garden)) $ filter (`Map.member` garden) ns
  where
    ns = [Point (x + 1) y, Point (x - 1) y, Point x (y + 1), Point x (y - 1)]

-- Part 2

countMoreSteps :: Int -> FilePath -> IO Int
countMoreSteps n filename = do
    file <- readFile filename
    let garden = gridMap (lines file)
    let (maxX, maxY) = findMaxXAndY (lines file)
    pure $ countNSteps' garden (maxX, maxY) n

countNSteps' :: Garden -> (Int, Int) -> Int -> Int
countNSteps' garden maxes n = Set.size $ iterate (nextSteps' garden maxes) (Set.singleton start) !! n
  where
    start = findStart garden

nextSteps' :: Garden -> (Int, Int) -> Set.Set Point -> Set.Set Point
nextSteps' garden maxes visited = Set.fromList $ concatMap (neighbours' garden maxes) (Set.toList visited)

neighbours' :: Garden -> (Int, Int) -> Point -> [Point]
neighbours' garden maxes (Point x y) = filter (isEmptyInInfiniteGarden garden maxes) ns
  where
    ns = [Point (x + 1) y, Point (x - 1) y, Point x (y + 1), Point x (y - 1)]

isEmptyInInfiniteGarden :: Garden -> (Int, Int) -> Point -> Bool
isEmptyInInfiniteGarden garden (maxX, maxY) (Point x y) = (/= '#') $ fromJust $ Map.lookup (Point (mod x maxX) (mod y maxY)) garden

findMaxXAndY :: [String] -> (Int, Int)
findMaxXAndY grid = (length grid, length (head grid))
