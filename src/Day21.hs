module Day21 (countSteps) where

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
