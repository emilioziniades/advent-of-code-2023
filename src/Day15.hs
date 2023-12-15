module Day15 (sumHashes) where

import Data.Char (isSpace, ord)
import Util.Lists (splitOn)

sumHashes :: FilePath -> IO Int
sumHashes filename = do
    file <- readFile filename
    pure $ sum $ hash <$> splitOn ',' (filter (not . isSpace) file)

hash :: String -> Int
hash = foldl (\n x -> ((n + ord x) * 17) `rem` 256) 0
