module Day14 (measureLoad, measureLoadWithCycles) where

import Data.Bifunctor
import Data.List
import Data.Maybe
import Util.Lists

data Direction = North | West | South | East
    deriving (Show, Eq, Ord, Enum)

-- Part 1

measureLoad :: FilePath -> IO Int
measureLoad filename = do
    file <- readFile filename
    pure $ totalLoad $ flip tilt North $ lines file

-- Part 2

measureLoadWithCycles :: FilePath -> IO Int
measureLoadWithCycles filename = do
    file <- readFile filename
    let input = lines file
    let directions = cycle [North .. East]
    let nCycles = 1000000000
    pure $ totalLoad $ tiltNTimesWithCycle (nCycles * 4) input directions

tiltNTimesWithCycle :: Int -> [String] -> [Direction] -> [String]
tiltNTimesWithCycle n grid directions =
    let
        (firstRepeat, uniqueCycles) = fromJust $ uncons $ tiltUntilCycle (pure grid) directions
        (gr, g0) = splitAt (1 + fromJust (elemIndex firstRepeat uniqueCycles)) uniqueCycles
        n0 = length g0
        nr = length gr
        r = div (n - n0) nr
        n1 = r * nr
        n2 = mod (n - n0) nr
        ds = take n2 $ drop ((n0 + n1) `rem` 4) directions
     in
        foldl tilt firstRepeat ds

tiltUntilCycle :: [[String]] -> [Direction] -> [[String]]
tiltUntilCycle grids@(g : _) (d : ds)
    | allUnique = tiltUntilCycle (tilt g d : grids) ds
    | otherwise = grids
  where
    allUnique = length (nub grids) == length grids
tiltUntilCycle _ _ = error "grids list should always grow"

-- Common to Part 1 and Part 2

tilt :: [String] -> Direction -> [String]
tilt grid North = transpose $ rollRowLeft <$> transpose grid
tilt grid West = rollRowLeft <$> grid
tilt grid South = transpose $ rollRowRight <$> transpose grid
tilt grid East = rollRowRight <$> grid

rollRowLeft :: String -> String
rollRowLeft row = intercalate "#" $ sortBy (flip compare) <$> splitOn '#' row

rollRowRight :: String -> String
rollRowRight row = intercalate "#" $ sort <$> splitOn '#' row

totalLoad :: [String] -> Int
totalLoad grid = sum $ uncurry (*) . first (length . filter (== 'O')) <$> zip grid (reverse [1 .. length grid])
