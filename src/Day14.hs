module Day14 (measureLoad) where

import Data.Bifunctor
import Data.List
import Util.Lists

measureLoad :: FilePath -> IO Int
measureLoad filename = do
    file <- readFile filename
    pure $ totalLoad $ tiltNorth $ lines file

tiltNorth :: [String] -> [String]
tiltNorth grid = transpose $ rollRowLeft <$> transpose grid
  where
    rollRowLeft :: String -> String -- TODO: reverse . sort instead of sortBy (flip compare)
    rollRowLeft row = intercalate "#" $ sortBy (flip compare) <$> splitOn '#' row

totalLoad :: [String] -> Int
totalLoad grid = sum $ uncurry (*) . first (length . filter (== 'O')) <$> zip grid (reverse [1 .. length grid])
