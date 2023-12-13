module Day11 (sumPaths) where

import Data.Bifunctor
import Data.List
import Util.Lists

data Point = Point {getX :: Int, getY :: Int}
    deriving (Show, Eq, Ord)

sumPaths :: FilePath -> IO Int
sumPaths filename = do
    file <- readFile filename
    let grid = transpose $ expandSpace $ transpose $ expandSpace $ lines file
    let stars = snd <$> filter ((== '#') . fst) (enumerateGrid grid)
    pure $ (`div` 2) $ sum $ manhattan <$> stars <*> stars

expandSpace :: [String] -> [String]
expandSpace [] = []
expandSpace (x : xs)
    | '#' `elem` x = x : expandSpace xs
    | otherwise = x : x : expandSpace xs

enumerateGrid :: [String] -> [(Char, Point)]
enumerateGrid grid = concat (enumerateRows grid)

enumerateRows :: [String] -> [[(Char, Point)]]
enumerateRows grid = fmap rowToPoints cells
  where
    rows = enumerate grid
    cells = fmap (first enumerate) rows
    rowToPoints (cs, row) = fmap (second (Point row)) cs

manhattan :: Point -> Point -> Int
manhattan (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)
