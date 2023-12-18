module Day18 (lagoonArea) where

import Data.Complex
import Data.List
import Util.Lists

data Direction = U | R | D | L
    deriving (Show, Read, Eq, Ord)

lagoonArea :: FilePath -> IO Int
lagoonArea filename = do
    file <- readFile filename
    -- print file
    let input = parseInput file
    let corners = getCorners input (0 :+ 0)
    let perimeter = getPerimeter corners
    let innerArea = shoelace corners
    print ("inner area" <> show innerArea)
    pure (pickTheorem innerArea perimeter)

parseInput :: String -> [(Direction, Int)]
parseInput file = parseRow . words <$> lines file
  where
    parseRow (d : i : _) = (read d, read i)
    parseRow _ = error "malformed input row"

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
getPerimeter corners = round $ sum $ distance <$> windows 2 corners

distance :: [Complex Int] -> Double
distance [r1 :+ i1, r2 :+ i2] = sqrt ((fromIntegral r1 - fromIntegral r2) ** 2 + (fromIntegral i1 - fromIntegral i2) ** 2)
distance _ = error "expecting a list with two complex numbers"

shoelace :: [Complex Int] -> Int
shoelace (r1 :+ i1 : r2 :+ i2 : xs) = (i2 + i1) * (r2 - r1) + shoelace (r2 :+ i2 : xs)
shoelace _ = 0

pickTheorem :: Int -> Int -> Int
pickTheorem innerArea perimeter = (innerArea + perimeter + 3) `div` 2
