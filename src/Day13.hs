module Day13 (findSymmetries, findSingleAsymmetries) where

import Data.List
import Util.Lists (splitOn)

-- Part 1

findSymmetries :: FilePath -> IO Int
findSymmetries filename = do
    file <- readFile filename
    let input = parseInput file
    pure $ sum $ findAsymmetrySummary 0 <$> input

-- Part 2

findSingleAsymmetries :: FilePath -> IO Int
findSingleAsymmetries filename = do
    file <- readFile filename
    let input = parseInput file
    pure $ sum $ findAsymmetrySummary 1 <$> input

-- Common to Part 1 and Part 2

findAsymmetrySummary :: Int -> [String] -> Int
findAsymmetrySummary nAsymmetries grid = findAsymmetry nAsymmetries possibleSymmetryT + 100 * findAsymmetry nAsymmetries possibleSymmetry
  where
    gridT = transpose grid
    possibleSymmetry = fmap (`splitAt` grid) [0 .. length grid]
    possibleSymmetryT = fmap (`splitAt` gridT) [0 .. length gridT]

findAsymmetry :: Int -> [([String], [String])] -> Int
findAsymmetry _ [] = 0
findAsymmetry n ((l1, l2) : ls)
    | bothNotEmpty && nAsymmetries == n = n1
    | otherwise = findAsymmetry n ls
  where
    n1 = length l1
    n2 = length l2
    smallerN = min n1 n2
    bothNotEmpty = smallerN > 0
    (bl1, bl2) = (drop (n1 - smallerN) l1, take smallerN l2)
    nAsymmetries = countAsymmetries (bl1, reverse bl2)

countAsymmetries :: ([String], [String]) -> Int
countAsymmetries (l1, l2) = length $ filter not $ concat $ zipWith (zipWith (==)) l1 l2

-- Input parsing

parseInput :: String -> [[String]]
parseInput file = splitOn "" (lines file)
