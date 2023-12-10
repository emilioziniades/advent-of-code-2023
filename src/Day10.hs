module Day10 (farthestLoopPoint) where

import Data.Bifunctor

import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Tuple
import Util.Lists
import Prelude hiding (Left, Right)

type Grid = Map.Map Point Char

data Point = Point {getX :: Int, getY :: Int}
    deriving (Show, Eq, Ord)

data Direction = Up | Right | Down | Left
    deriving (Show, Eq, Ord)

-- Part 1

farthestLoopPoint :: FilePath -> IO Int
farthestLoopPoint filename = do
    file <- readFile filename
    let grid = parseInput file
    let start = findStart grid
    let (d1, d2) = findStartDirections grid start
    pure $ walkPipe grid ((start, d1), (start, d2)) 0

walkPipe :: Grid -> ((Point, Direction), (Point, Direction)) -> Int -> Int
walkPipe grid ((pt1, d1), (pt2, d2)) n
    | pt1 == pt2 && n /= 0 = n
    | otherwise = walkPipe grid (step grid (pt1, d1), step grid (pt2, d2)) (n + 1)

step :: Grid -> (Point, Direction) -> (Point, Direction)
step grid (point, direction) = (nextPoint, nextDirection pipe direction)
  where
    nextPoint = neighbour point direction
    pipe = fromJust $ Map.lookup nextPoint grid

nextDirection :: Char -> Direction -> Direction
nextDirection pipe direction = case (pipe, direction) of
    ('-', Left) -> Left
    ('-', Right) -> Right
    ('|', Up) -> Up
    ('|', Down) -> Down
    ('L', Down) -> Right
    ('L', Left) -> Up
    ('J', Down) -> Left
    ('J', Right) -> Up
    ('7', Right) -> Down
    ('7', Up) -> Left
    ('F', Left) -> Down
    ('F', Up) -> Right
    _ -> error (pipe : " " <> show direction <> " : invalid state")

findStart :: Grid -> Point
findStart grid = fst $ fromJust $ find ((== 'S') . snd) (Map.assocs grid)

findStartDirections :: Grid -> Point -> (Direction, Direction)
findStartDirections grid start = case snd <$> filter (isEntryPossible grid) (neighbours grid start) of
    [d1, d2] -> (d1, d2)
    _ -> error "there should only be two start directions"

isEntryPossible :: Grid -> (Point, Direction) -> Bool
isEntryPossible grid (destination, entryDirection) = case (fromJust $ Map.lookup destination grid, entryDirection) of
    ('|', Up) -> True
    ('|', Down) -> True
    ('-', Left) -> True
    ('-', Right) -> True
    ('L', Down) -> True
    ('L', Left) -> True
    ('J', Down) -> True
    ('J', Right) -> True
    ('7', Up) -> True
    ('7', Right) -> True
    ('F', Up) -> True
    ('F', Left) -> True
    _ -> False

neighbours :: Grid -> Point -> [(Point, Direction)]
neighbours grid (Point x y) =
    filter
        ((`Map.member` grid) . fst)
        [ (Point (x + 1) y, Down)
        , (Point (x - 1) y, Up)
        , (Point x (y + 1), Right)
        , (Point x (y - 1), Left)
        ]

neighbour :: Point -> Direction -> Point
neighbour (Point x y) direction = case direction of
    Up -> Point (x - 1) y
    Right -> Point x (y + 1)
    Down -> Point (x + 1) y
    Left -> Point x (y - 1)

-- Input parsing

parseInput :: String -> Grid
parseInput file = Map.fromList $ swap <$> points
  where
    rows = enumerate (lines file)
    cells = fmap (first enumerate) rows
    points = concatMap rowToPoints cells
    rowToPoints (cs, row) = fmap (second (Point row)) cs
