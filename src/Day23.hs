module Day23 (findLongestHike, findLongestHikeNoSlope) where

import qualified Data.Set as Set
import GHC.Arr (Array, bounds, listArray, (!))
import Prelude hiding (lookup)

type Grid = Array Int (Array Int Tile)

data Tile = Path | Forest | SteepUp | SteepRight | SteepDown | SteepLeft
    deriving (Show, Eq)

data Position = Position {getX :: Int, getY :: Int}
    deriving (Show, Eq, Ord)

data State = State {getCurrent :: Position, getVisited :: Set.Set Position}
    deriving (Show)

type GetNeighbours = Grid -> Position -> [Position]

-- Part 1

findLongestHike :: String -> Int
findLongestHike file = longestHike grid getNeighboursWithSlope endPosition startState
  where
    grid = parseInput file
    endPosition = getEndPosition grid
    startState = State (Position 0 1) Set.empty

getNeighboursWithSlope :: GetNeighbours
getNeighboursWithSlope grid position = case tile of
    SteepUp -> [Position (x - 1) y]
    SteepRight -> [Position x (y + 1)]
    SteepDown -> [Position (x + 1) y]
    SteepLeft -> [Position x (y - 1)]
    Path -> neighbours position
    Forest -> error "current position can never be a forest"
  where
    tile = lookup grid position
    x = getX position
    y = getY position

-- Part 2

findLongestHikeNoSlope :: String -> Int
findLongestHikeNoSlope file = longestHike grid getNeighboursNoSlope endPosition startState
  where
    grid = parseInput file
    endPosition = getEndPosition grid
    startState = State (Position 0 1) Set.empty

getNeighboursNoSlope :: GetNeighbours
getNeighboursNoSlope _ = neighbours

-- Common to Part 1 and 2

longestHike :: Grid -> GetNeighbours -> Position -> State -> Int
longestHike grid getNeighbours endPosition (State position visited)
    | position == endPosition = 0
    | null nextNeighbours = 0 -- TODO: this allows illegal states, figure out how to abort calculation if dead-end reached
    | otherwise = (+ 1) $ maximum $ fmap nextStates nextNeighbours
  where
    nextNeighbours = filter (isPath grid) $ filter (`Set.notMember` visited) $ filter (isInGrid grid) (getNeighbours grid position)
    nextVisited = Set.insert position visited
    nextStates p = longestHike grid getNeighbours endPosition (State p nextVisited)

-- General grid logic

lookup :: Grid -> Position -> Tile
lookup array (Position x y) = array ! x ! y

isPath :: Grid -> Position -> Bool
isPath grid position = tile `elem` [Path, SteepUp, SteepRight, SteepDown, SteepLeft]
  where
    tile = lookup grid position

isInGrid :: Grid -> Position -> Bool
isInGrid grid (Position x y) = x >= minX && x <= maxX && y >= minY && y <= maxY
  where
    (minX, maxX) = bounds grid
    (minY, maxY) = bounds (grid ! 0)

neighbours :: Position -> [Position]
neighbours (Position x y) =
    [ Position x (y + 1)
    , Position x (y - 1)
    , Position (x + 1) y
    , Position (x - 1) y
    ]

getEndPosition :: Grid -> Position
getEndPosition grid = Position (snd . bounds $ grid) (snd . bounds $ grid ! 0)

-- Input parsing

parseInput :: String -> Grid
parseInput file = makeArray <$> makeArray ((fmap . fmap) newTile (lines file))

newTile :: Char -> Tile
newTile '.' = Path
newTile '#' = Forest
newTile '^' = SteepUp
newTile '>' = SteepRight
newTile 'v' = SteepDown
newTile '<' = SteepLeft
newTile c = error (c : ": not a valid tile")

makeArray :: [a] -> Array Int a
makeArray xs = listArray (0, length xs - 1) xs
