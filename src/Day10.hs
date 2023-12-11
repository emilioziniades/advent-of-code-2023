module Day10 (farthestLoopPoint, countEnclosingLoop) where

import Data.Bifunctor

import Data.Bits
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
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

-- Part 2

countEnclosingLoop :: FilePath -> IO Int
countEnclosingLoop filename = do
    file <- readFile filename
    let grid = parseInput file
    let start = findStart grid
    let ds = findStartDirections grid start
    let allLoopPipes = getAllLoopPipes grid start (fst ds)
    let enumeratedRows = getEnumeratedRows (replaceSWithPipe file ds)
    let rowCounts = findEnclosedInRow False allLoopPipes <$> enumeratedRows
    print rowCounts
    pure $ sum rowCounts

replaceSWithPipe :: String -> (Direction, Direction) -> String
replaceSWithPipe file directions = fmap replaceS file
  where
    replaceS c = if c == 'S' then pipeShape else c
    pipeShape = getPipeShape directions

getAllLoopPipes :: Grid -> Point -> Direction -> Set.Set Point
getAllLoopPipes grid start direction = Set.fromList $ walkAllPipes grid start True (start, direction)

walkAllPipes :: Grid -> Point -> Bool -> (Point, Direction) -> [Point]
walkAllPipes grid startPoint atStart (point, direction)
    | startPoint == point && not atStart = []
    | otherwise = point : walkAllPipes grid startPoint False (step grid (point, direction))

findEnclosedInRow :: Bool -> Set.Set Point -> [(Char, Point)] -> Int
findEnclosedInRow _ _ [] = 0
findEnclosedInRow isInside loopPipes row@(r : rs)
    | isInside && not isLoopPipe = 1 + findEnclosedInRow isInside loopPipes rs
    | not isInside && not isLoopPipe = 0 + findEnclosedInRow isInside loopPipes rs
    | isLoopPipe && fst r == '|' = 0 + findEnclosedInRow (not isInside) loopPipes rs
    | otherwise = 0 + findEnclosedInRow (xor (crossedEdge loopSegment) isInside) loopPipes rest
  where
    isLoopPipe = Set.member (snd r) loopPipes
    (loopSegment, rest) = nextPipeSegment loopPipes row

crossedEdge :: [(Char, Point)] -> Bool
crossedEdge pipeSegment = case (fst $ head pipeSegment, fst $ last pipeSegment) of
    ('F', '7') -> False
    ('L', 'J') -> False
    ('F', 'J') -> True
    ('L', '7') -> True
    _ -> error $ show pipeSegment <> " unrecognized pipe sequence"

--
nextPipeSegment :: Set.Set Point -> [(Char, Point)] -> ([(Char, Point)], [(Char, Point)])
nextPipeSegment loopPipes (current@('|', pt) : rest) | Set.member pt loopPipes = (pure current, rest)
nextPipeSegment loopPipes row = span isHorizontalLoopPipe row
  where
    isHorizontalLoopPipe (c, pt) = hasHorizontalComponent c && Set.member pt loopPipes

hasHorizontalComponent :: Char -> Bool
hasHorizontalComponent c = case c of
    '-' -> True
    'J' -> True
    'L' -> True
    '7' -> True
    'F' -> True
    '|' -> False
    '.' -> False
    _ -> error (c : ": unrecognized pipe shape")

getEnumeratedRows :: String -> [[(Char, Point)]]
getEnumeratedRows file = fmap rowToPoints cells
  where
    rows = enumerate (lines file)
    cells = fmap (first enumerate) rows
    rowToPoints (cs, row) = fmap (second (Point row)) cs

getPipeShape :: (Direction, Direction) -> Char
getPipeShape directions = case directions of
    (Up, Down) -> '|'
    (Down, Up) -> '|'
    (Left, Right) -> '-'
    (Right, Left) -> '-'
    (Up, Right) -> 'L'
    (Right, Up) -> 'L'
    (Up, Left) -> 'J'
    (Left, Up) -> 'J'
    (Down, Left) -> '7'
    (Left, Down) -> '7'
    (Down, Right) -> 'F'
    (Right, Down) -> 'F'
    _ -> error $ "don't know how to replace " <> show directions

-- Input parsing

parseInput :: String -> Grid
parseInput file = Map.fromList $ swap <$> points
  where
    rows = enumerate (lines file)
    cells = fmap (first enumerate) rows
    points = concatMap rowToPoints cells
    rowToPoints (cs, row) = fmap (second (Point row)) cs
