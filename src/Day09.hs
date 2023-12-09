module Day09 (extrapolateValues, extrapolateValuesBackwards) where

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
    | otherwise = findDifferences $ fmap (foldr1 (flip (-))) (windowsN 2 l) : lists

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

-- Utility

-- whereas chunksN do not overlap, these do.
windowsN :: Int -> [a] -> [[a]]
windowsN n xs
    | length xs < n = []
    | otherwise = take n xs : windowsN n (tail xs)
