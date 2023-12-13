module Day13 (findSymmetries) where

import Data.List
import Util.Lists (splitOn)

-- Part 1

findSymmetries :: FilePath -> IO Int
findSymmetries filename = do
    file <- readFile filename
    let input = parseInput file
    pure $ sum $ findSymmetrySummary <$> input

findSymmetrySummary :: [String] -> Int
findSymmetrySummary grid = findSymmetry possibleSymmetryT + 100 * findSymmetry possibleSymmetry
  where
    gridT = transpose grid
    possibleSymmetry = fmap (`splitAt` grid) [0 .. length grid]
    possibleSymmetryT = fmap (`splitAt` gridT) [0 .. length gridT]

findSymmetry :: [([String], [String])] -> Int
findSymmetry [] = 0
findSymmetry ((l1, l2) : ls)
    | bothNotEmpty && isSymmetrical = n1
    | otherwise = findSymmetry ls
  where
    n1 = length l1
    n2 = length l2
    smallerN = min n1 n2
    bothNotEmpty = smallerN > 0
    (bl1, bl2) = (drop (n1 - smallerN) l1, take smallerN l2)
    isSymmetrical = bl1 == reverse bl2

-- Input parsing

parseInput :: String -> [[String]]
parseInput file = splitOn "" (lines file)
