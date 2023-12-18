module Day18 (lagoonArea, hexLagoonArea) where

import Data.Char (isAlphaNum)
import Data.Complex
import Numeric
import Util.Lists

data Direction = U | R | D | L
    deriving (Show, Read, Eq, Ord)

-- Part 1

lagoonArea :: FilePath -> IO Int
lagoonArea filename = do
    file <- readFile filename
    pure $ measureLagoonArea (parseInput file) (0 :+ 0)

parseInput :: String -> [(Direction, Int)]
parseInput file = parseRow . words <$> lines file
  where
    parseRow (d : i : _) = (read d, read i)
    parseRow _ = error "malformed input row"

-- Part 2

hexLagoonArea :: FilePath -> IO Int
hexLagoonArea filename = do
    file <- readFile filename
    pure $ measureLagoonArea (parseHexInput file) (0 :+ 0)

parseHexInput :: String -> [(Direction, Int)]
parseHexInput file = parseHexRow <$> lines file

parseHexRow :: String -> (Direction, Int)
parseHexRow row = (directionFromChar c, fst $ head $ readHex ns)
  where
    hex = filter isAlphaNum $ last $ words row
    (ns, c) = (init hex, last hex)
    directionFromChar char = case char of
        '0' -> R
        '1' -> D
        '2' -> L
        '3' -> U
        _ -> error "char cannot become direction"

-- Common to Part 1 and 2

measureLagoonArea :: [(Direction, Int)] -> Complex Int -> Int
measureLagoonArea directions start = pickTheorem innerArea perimeter
  where
    corners = getCorners directions start
    perimeter = getPerimeter corners
    innerArea = shoelace corners

getStep :: Direction -> Int -> Complex Int
getStep U n = 0 :+ n
getStep D n = 0 :+ (-n)
getStep R n = n :+ 0
getStep L n = (-n) :+ 0

add :: Complex Int -> Complex Int -> Complex Int
add (r1 :+ i1) (r2 :+ i2) = (r1 + r2) :+ (i1 + i2)

getCorners :: [(Direction, Int)] -> Complex Int -> [Complex Int]
getCorners steps start = scanl step start steps
  where
    step :: Complex Int -> (Direction, Int) -> Complex Int
    step pt (d, i) = getStep d i `add` pt

getPerimeter :: [Complex Int] -> Int
getPerimeter corners = sum $ distance <$> windows 2 corners

distance :: [Complex Int] -> Int
distance [r1 :+ i1, r2 :+ i2]
    | r1 == r2 = abs (i1 - i2)
    | i1 == i2 = abs (r1 - r2)
distance _ = error "expecting a list with two complex numbers"

shoelace :: [Complex Int] -> Int
shoelace (r1 :+ i1 : r2 :+ i2 : xs) = (i2 + i1) * (r2 - r1) + shoelace (r2 :+ i2 : xs)
shoelace _ = 0

pickTheorem :: Int -> Int -> Int
pickTheorem innerArea perimeter = (innerArea + perimeter + 3) `div` 2
