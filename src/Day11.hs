module Day11 (sumPaths) where

import Data.List
import Util.Grid
import Util.Lists

sumPaths :: Int -> FilePath -> IO Int
sumPaths expansionFactor filename = do
    file <- readFile filename
    let grid = lines file
    let expansionRows = expansions grid
    let expansionCols = expansions (transpose grid)
    let stars = adjustStar expansionFactor expansionRows expansionCols . fst <$> filter ((== '#') . snd) (enumerateGrid grid)
    pure $ (`div` 2) $ sum $ manhattan <$> stars <*> stars

expansions :: [String] -> [Int]
expansions grid = snd <$> filter (('#' `notElem`) . fst) (enumerate grid)

adjustStar :: Int -> [Int] -> [Int] -> Point -> Point
adjustStar expansionFactor expansionRows expansionCols (Point x y) = Point (x + (expansionFactor - 1) * rowExps) (y + (expansionFactor - 1) * colExps)
  where
    rowExps = length $ takeWhile (< x) expansionRows
    colExps = length $ takeWhile (< y) expansionCols
