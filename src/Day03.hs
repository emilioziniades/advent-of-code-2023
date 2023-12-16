module Day03 (sumPartNumbers, sumGearRatios) where

import Data.Char (isNumber)
import Data.List
import qualified Data.Map as Map
import Util.Grid
import Util.Lists

data Range = Range Point Point
    deriving (Show)

type Grid = [[Char]]
type Row = [Char]

data PartNumber = PartNumber {number :: Int, _range :: Range}

type GearMap = Map.Map Point [Int]

-- Part 1

sumPartNumbers :: FilePath -> IO Int
sumPartNumbers filename = do
    file <- readFile filename
    let grid = words file
    pure $ sum $ number <$> filter (isPartNumber grid) (getPartNumbers grid)

isPartNumber :: Grid -> PartNumber -> Bool
isPartNumber grid (PartNumber _ range) = any isSymbol rangeNeighbours
  where
    rangeNeighbours = fst <$> getRangeNeighbours grid range

isSymbol :: Char -> Bool
isSymbol c = (not . isNumber) c && c /= '.'

-- Part 2

sumGearRatios :: FilePath -> IO Int
sumGearRatios filename = do
    file <- readFile filename
    let grid = words file
    let partNumbers = getPartNumbers grid
    let gearPartNumbers = collectGearPartNumbers grid partNumbers
    pure $ sum $ fmap product $ filter ((== 2) . length) $ Map.elems gearPartNumbers

collectGearPartNumbers :: Grid -> [PartNumber] -> GearMap
collectGearPartNumbers grid = foldr (foldPartNumber grid) Map.empty

foldPartNumber :: Grid -> PartNumber -> GearMap -> GearMap
foldPartNumber grid (PartNumber n range) hashMap =
    let
        partNumberNeighbours = getRangeNeighbours grid range
        isGear (c, _) = c == '*'
        gear = find isGear partNumberNeighbours
     in
        case gear of
            Just (_, pt) -> Map.insertWith (++) pt [n] hashMap
            Nothing -> hashMap

-- Used across both Part 1 and Part 2

getPartNumbers :: Grid -> [PartNumber]
getPartNumbers grid = concatMap getPartNumbersInRow (enumerate grid)

getPartNumbersInRow :: (Row, Int) -> [PartNumber]
getPartNumbersInRow (row, x) =
    fmap (makePartNumber x) $
        filter (isNumber . fst . head) $
            groupBy areBothNumbers (enumerate row)

makePartNumber :: Int -> [(Char, Int)] -> PartNumber
makePartNumber x ns = PartNumber (makeNumber ns) (makeRange x ns)

makeNumber :: [(Char, Int)] -> Int
makeNumber ns = read $ fst <$> ns

makeRange :: Int -> [(Char, Int)] -> Range
makeRange x ns = Range (Point x ((snd . head) ns)) (Point x ((snd . last) ns))

areBothNumbers :: (Char, a) -> (Char, a) -> Bool
areBothNumbers (x, _) (y, _) = isNumber x && isNumber y

-- generic grid logic

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

gridNeighbours :: Grid -> Point -> [(Char, Point)]
gridNeighbours grid point = gridLookup grid <$> filter (isInBounds grid) (neighbours point)

gridLookup :: Grid -> Point -> (Char, Point)
gridLookup grid pt@(Point x y) = (c, pt)
  where
    c = (grid !! x) !! y

isInBounds :: Grid -> Point -> Bool
isInBounds grid (Point x y) = x >= 0 && x < maxX && y >= 0 && y < maxY
  where
    maxX = length grid
    maxY = (length . head) grid

getRangeNeighbours :: Grid -> Range -> [(Char, Point)]
getRangeNeighbours grid range = concatMap (gridNeighbours grid) (getPoints range)
