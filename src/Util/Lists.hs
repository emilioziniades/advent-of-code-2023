module Util.Lists (splitOn, splitOn2, enumerate, chunks, windows) where

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn e list
    | e `notElem` list = [list]
    | e `elem` l2 = l1 : splitOn e l2
    | otherwise = [l1, l2]
  where
    (l1, l2) = splitOn2 e list

splitOn2 :: (Eq a) => a -> [a] -> ([a], [a])
splitOn2 e list = case span (/= e) list of
    (as, _ : bs) -> (as, bs)
    (as, []) -> (as, [])

enumerate :: [a] -> [(a, Int)]
enumerate arr = zip arr [0 ..]

-- non-overlapping subsequences of [a]
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

-- overlapping subsequences of [a]
windows :: Int -> [a] -> [[a]]
windows n xs
    | length xs < n = []
    | otherwise = take n xs : windows n (tail xs)
