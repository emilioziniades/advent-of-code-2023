module Day18 (lagoonArea, hexLagoonArea) where

import Data.Char (isAlphaNum)
import Data.Complex
import Numeric

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

directionFromChar :: Char -> Direction
directionFromChar char = case char of
    '0' -> R
    '1' -> D
    '2' -> L
    '3' -> U
    _ -> error (char : ": cannot become direction")

-- Common to Part 1 and 2

measureLagoonArea :: [(Direction, Int)] -> Complex Int -> Int
measureLagoonArea directions start = totalArea innerArea perimeter
  where
    corners = getCorners directions start
    perimeter = getPerimeter directions
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

getPerimeter :: [(Direction, Int)] -> Int
getPerimeter instructions = sum $ snd <$> instructions

-- Shoelace formula: https://en.wikipedia.org/wiki/Shoelace_formula
shoelace :: [Complex Int] -> Int
shoelace (r1 :+ i1 : r2 :+ i2 : xs) = (i2 + i1) * (r2 - r1) `div` 2 + shoelace (r2 :+ i2 : xs)
shoelace _ = 0

-- Pick's Theorem: https://en.wikipedia.org/wiki/Pick%27s_theorem
-- A = total area, i = inner area, b = perimeter
-- A = i + (b/2) - 1
-- We have A from `shoelace` above, and b from calculating the perimeter, we want i
-- i = A - (b/2) + 1
-- Now, we have to add the perimeter back, since the question asks for the entire area
-- answer = i + b
-- answer = A - (b/2) + 1 + b
-- answer = A + (b/2) + 1
totalArea :: Int -> Int -> Int
totalArea area perimeter = area + perimeter `div` 2 + 1
