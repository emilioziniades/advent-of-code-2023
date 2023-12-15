module Day14 (measureLoad, measureLoadWithCycles) where

import Data.Bifunctor
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Util.Lists

data Direction = North | West | South | East
    deriving (Show, Eq, Ord, Enum)

-- Part 1

measureLoad :: FilePath -> IO Int
measureLoad filename = do
    file <- readFile filename
    pure $ totalLoad $ tilt North $ lines file

-- Part 2

measureLoadWithCycles :: FilePath -> IO Int
measureLoadWithCycles filename = do
    file <- readFile filename
    let nCycles = 1000000000
    pure $ totalLoad $ tiltNTimesWithCycle nCycles (lines file)

tiltNTimesWithCycle :: Int -> [String] -> [String]
tiltNTimesWithCycle n grid = iterate tiltCycle firstRepeat !! (n2 - 1)
  where
    (firstRepeat, uniqueCycles) = fromJust $ uncons $ tiltUntilCycle Map.empty 0 grid
    (gr, g0) = splitAt (1 + fromJust (elemIndex firstRepeat uniqueCycles)) uniqueCycles
    n0 = length g0
    nr = length gr
    n2 = mod (n - n0) nr

tiltUntilCycle :: Map.Map [String] Int -> Int -> [String] -> [[String]]
tiltUntilCycle cache n g
    | seenBefore = newG : fmap fst (sortBy (\(_, a) (_, b) -> compare b a) $ Map.toList cache)
    | otherwise = tiltUntilCycle (Map.insert newG n cache) (n + 1) newG
  where
    newG = tiltCycle g
    seenBefore = Map.member newG cache

tiltCycle :: [String] -> [String]
tiltCycle grid = tilt East $ tilt South $ tilt West $ tilt North grid

-- Common to Part 1 and Part 2

tilt :: Direction -> [String] -> [String]
tilt North grid = transpose $ rollRowLeft <$> transpose grid
tilt West grid = rollRowLeft <$> grid
tilt South grid = transpose $ rollRowRight <$> transpose grid
tilt East grid = rollRowRight <$> grid

rollRowLeft :: String -> String
rollRowLeft row = intercalate "#" $ sortBy (flip compare) <$> splitOn '#' row

rollRowRight :: String -> String
rollRowRight row = intercalate "#" $ sort <$> splitOn '#' row

totalLoad :: [String] -> Int
totalLoad grid = sum $ uncurry (*) . first (length . filter (== 'O')) <$> zip grid (reverse [1 .. length grid])
