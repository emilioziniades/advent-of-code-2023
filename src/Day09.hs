module Day09 (extrapolateValues) where

-- Part 1

extrapolateValues :: FilePath -> IO Int
extrapolateValues filename = do
    file <- readFile filename
    let seqs = parseInput file
    pure $ sum $ extrapolateValue <$> seqs

extrapolateValue :: [Int] -> Int
extrapolateValue s = sum $ last <$> findDifferences (pure s)

findDifferences :: [[Int]] -> [[Int]]
findDifferences [] = error "cannot find differences of an empty list"
findDifferences lists@(l : _)
    | all (== 0) l = lists
    | otherwise = findDifferences $ fmap (foldr1 (flip (-))) (windowsN 2 l) : lists

-- Input parsing

parseInput :: String -> [[Int]]
parseInput file = fmap (fmap read) $ words <$> lines file

-- Utility

-- whereas chunksN do not overlap, these do.
windowsN :: Int -> [a] -> [[a]]
windowsN n xs
    | length xs < n = []
    | otherwise = take n xs : windowsN n (tail xs)

windows2 :: [a] -> [(a, a)]
windows2 (x1 : x2 : xs) = (x1, x2) : windows2 xs
windows2 [_] = []
windows2 [] = []
