module Day03 (sumPartNumbers) where

import Data.Char (isNumber)
import Data.List

data Point = Point {getX :: Int, getY :: Int}
    deriving (Show)

data Range = Range Point Point
    deriving (Show)

type Grid = [[Char]]
type Row = [Char]

data PartNumber = PartNumber {number :: Int, _range :: Range}

-- Part 1

sumPartNumbers :: String -> IO Int
sumPartNumbers filename = do
    file <- readFile filename
    let array = words file
    pure $ sum $ number <$> filter (isPartNumber array) (getPartNumbers array)

getPartNumbers :: Grid -> [PartNumber]
getPartNumbers array =
    let
        makeNumbers (row, x) = getPartNumbersInRow x row
     in
        mconcat $ fmap makeNumbers (enumerate array)

getPartNumbersInRow :: Int -> Row -> [PartNumber]
getPartNumbersInRow x row =
    let
        groups = groupBy areBothNumbers (enumerate row)
        numbers = filter (isNumber . fst . head) groups
     in
        fmap (makePartNumber x) numbers

makeRange :: Int -> [(Char, Int)] -> Range
makeRange x ns = Range (Point x ((snd . head) ns)) (Point x ((snd . last) ns))

makeNumber :: [(Char, Int)] -> Int
makeNumber ns = read $ fst <$> ns

makePartNumber :: Int -> [(Char, Int)] -> PartNumber
makePartNumber x ns = PartNumber (makeNumber ns) (makeRange x ns)

areBothNumbers :: (Char, a) -> (Char, a) -> Bool
areBothNumbers (x, _) (y, _) = isNumber x && isNumber y

isPartNumber :: Grid -> PartNumber -> Bool
isPartNumber array (PartNumber _ range) = any isSymbol rangeNeighbours
  where
    rangeNeighbours = getRangeNeighbours array range

isSymbol :: Char -> Bool
isSymbol c = (not . isNumber) c && c /= '.'

-- generic array logic

getPoints :: Range -> [Point]
getPoints (Range start end) = fmap (Point x1) [y1 .. y2]
  where
    x1 = getX start
    y1 = getY start
    y2 = getY end

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
arrayNeighbours array point = arrayLookup array <$> filter (isInBounds array) (neighbours point)

arrayLookup :: Grid -> Point -> Char
arrayLookup array (Point x y) = (array !! x) !! y

isInBounds :: Grid -> Point -> Bool
isInBounds array (Point x y) = x >= 0 && x < maxX && y >= 0 && y < maxY
  where
    maxX = length array
    maxY = (length . head) array

getRangeNeighbours :: Grid -> Range -> [Char]
getRangeNeighbours array range = mconcat $ fmap (arrayNeighbours array) (getPoints range)

-- utility

enumerate :: [a] -> [(a, Int)]
enumerate arr = zip arr [0 ..]
