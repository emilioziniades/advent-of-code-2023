module Parse (splitOn, splitOn2) where

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
