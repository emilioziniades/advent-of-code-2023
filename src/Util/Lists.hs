module Util.Lists (splitOn, splitOn2, enumerate, windowsN) where

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

windowsN :: Int -> [a] -> [[a]]
windowsN _ [] = []
windowsN n xs = take n xs : windowsN n (drop n xs)
