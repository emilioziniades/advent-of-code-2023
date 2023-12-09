module Day09 (extrapolateValues, extrapolateValuesBackwards) where

import Util.Lists

-- Part 1

extrapolateValues :: FilePath -> IO Int
extrapolateValues filename = do
    file <- readFile filename
    pure $ sum $ extrapolateValue <$> parseInput file

extrapolateValue :: [Int] -> Int
extrapolateValue s = sum $ last <$> findDifferences (pure s)

findDifferences :: [[Int]] -> [[Int]]
findDifferences [] = error "cannot find differences of an empty list"
findDifferences lists@(l : _)
    | all (== 0) l = lists
    | otherwise = findDifferences $ fmap (foldr1 (flip (-))) (windows 2 l) : lists

-- Part 2

extrapolateValuesBackwards :: FilePath -> IO Int
extrapolateValuesBackwards filename = do
    file <- readFile filename
    pure $ sum $ extrapolateValueBackwards <$> parseInput file

extrapolateValueBackwards :: [Int] -> Int
extrapolateValueBackwards s = foldl1 (flip (-)) $ head <$> findDifferences (pure s)

-- Input parsing

parseInput :: String -> [[Int]]
parseInput file = fmap (fmap read) $ words <$> lines file
