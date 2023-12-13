module Day11 (sumPaths) where

import Data.Bifunctor
import Data.List
import Util.Lists

data Point = Point {getX :: Int, getY :: Int}
    deriving (Show, Eq, Ord)

sumPaths :: Int -> FilePath -> IO Int
sumPaths expansionFactor filename = do
    file <- readFile filename
    let grid = lines file
    let expansionRows = expansions grid
    let expansionCols = expansions (transpose grid)
    let stars = adjustStar expansionFactor expansionRows expansionCols . snd <$> filter ((== '#') . fst) (enumerateGrid grid)
    pure $ (`div` 2) $ sum $ manhattan <$> stars <*> stars

expansions :: [String] -> [Int]
expansions grid = snd <$> filter (('#' `notElem`) . fst) (enumerate grid)

adjustStar :: Int -> [Int] -> [Int] -> Point -> Point
adjustStar expansionFactor expansionRows expansionCols (Point x y) = Point (x + (expansionFactor - 1) * rowExps) (y + (expansionFactor - 1) * colExps)
  where
    rowExps = length $ takeWhile (< x) expansionRows
    colExps = length $ takeWhile (< y) expansionCols

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
