module Day03 (sumPartNumbers) where

import Data.Char (isNumber)
import Data.List

data Point = Point {getX :: Int, getY :: Int}
    deriving (Show)

data Range = Range Point Point
    deriving (Show)

type Grid = [[Char]]
type Row = [Char]

sumPartNumbers :: String -> IO Int
sumPartNumbers filename = do
    file <- readFile filename
    let array = words file
    pure $ sum $ fst <$> filter (isPartNumber array) (getNumbers array)

getNumbers :: Grid -> [(Int, Range)]
getNumbers array =
    let
        makeNumbers (row, x) = getNumberInRow x row
     in
        mconcat $ fmap makeNumbers (enumerate array)

getNumberInRow :: Int -> Row -> [(Int, Range)]
getNumberInRow x row =
    let
        gRow = groupBy bothNumbers (enumerate row)
        numbers = filter (isNumber . fst . head) gRow
        bothNumbers (a, _) (b, _) = isNumber a && isNumber b
        getRange :: [(Char, Int)] -> Range
        getRange digits = Range (Point x ((snd . head) digits)) (Point x ((snd . last) digits))
        getNumber :: [(Char, Int)] -> Int
        getNumber digits = read $ fst <$> digits
        makeNumbers number = (getNumber number, getRange number)
     in
        fmap makeNumbers numbers

isPartNumber :: Grid -> (Int, Range) -> Bool
isPartNumber array (_, range) =
    let
        rangeNeighbours = getRangeNeighbours array range
        isSymbol c = not (isNumber c) && c /= '.'
     in
        any isSymbol rangeNeighbours

getRangeNeighbours :: Grid -> Range -> [Char]
getRangeNeighbours array (Range start end) =
    let
        points = fmap (Point (getX start)) [getY start .. getY end]
     in
        mconcat $ fmap (arrayNeighbours array) points

neighbours :: Point -> [Point]
neighbours point =
    [ Point (x - 1) (y - 1)
    , Point x (y - 1)
    , Point (x + 1) (y - 1)
    , Point (x - 1) y
    , Point (x + 1) y
    , Point (x - 1) (y + 1)
    , Point x (y + 1)
    , Point (x + 1) (y + 1)
    ]
  where
    x = getX point
    y = getY point

arrayNeighbours :: Grid -> Point -> [Char]
arrayNeighbours array (Point x y) =
    neighbourLookup array <$> filter isInBounds (neighbours (Point x y))
  where
    maxX = length array
    maxY = length (head array)
    neighbourLookup arr pt = (arr !! getX pt) !! getY pt
    isInBounds pt = getX pt >= 0 && getX pt < maxX && getY pt >= 0 && getY pt < maxY

enumerate :: [a] -> [(a, Int)]
enumerate arr = zip arr [0 ..]
