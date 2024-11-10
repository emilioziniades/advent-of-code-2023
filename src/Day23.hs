module Day23 (findLongestHike, findLongestHikeNoSlope) where

import Data.Maybe (fromJust)
import qualified Data.Set as Set
import GHC.Arr (Array, bounds, listArray, (!))
import Prelude hiding (lookup)

type Position = (Int, Int)

type Grid = Array Int (Array Int Tile)

type GetNeighbours = Grid -> Position -> [Position]

data Tile = Path | Forest | SteepUp | SteepRight | SteepDown | SteepLeft
    deriving (Show, Eq)

data State = State {getCurrent :: Position, getVisited :: Set.Set Position}
    deriving (Show)

-- Part 1

findLongestHike :: String -> Int
findLongestHike file = fromJust $ longestHike grid getNeighboursWithSlope (getEndPosition grid) (State (0, 1) Set.empty)
  where
    grid = parseInput file

getNeighboursWithSlope :: GetNeighbours
getNeighboursWithSlope grid position = case tile of
    SteepUp -> [(x - 1, y)]
    SteepRight -> [(x, y + 1)]
    SteepDown -> [(x + 1, y)]
    SteepLeft -> [(x, y - 1)]
    Path -> neighbours position
    Forest -> error "current position can never be a forest"
  where
    tile = lookup grid position
    x = fst position
    y = snd position

-- Part 2

findLongestHikeNoSlope :: String -> Int
findLongestHikeNoSlope file = fromJust $ longestHike grid getNeighboursNoSlope (getEndPosition grid) (State (0, 1) Set.empty)
  where
    grid = parseInput file

getNeighboursNoSlope :: GetNeighbours
getNeighboursNoSlope _ = neighbours

-- Common to Part 1 and 2

longestHike :: Grid -> GetNeighbours -> Position -> State -> Maybe Int
longestHike grid getNeighbours endPosition (State position visited)
    | position == endPosition = Just 0
    | null nextNeighbours = Nothing
    | otherwise = ((+) <$> Just 1 <*>) $ maximum $ fmap nextStates nextNeighbours
  where
    nextNeighbours = filter (isPath grid) $ filter (`Set.notMember` visited) $ filter (isInGrid grid) (getNeighbours grid position)
    nextVisited = Set.insert position visited
    nextStates p = longestHike grid getNeighbours endPosition (State p nextVisited)

-- General grid logic

lookup :: Grid -> Position -> Tile
lookup array (x, y) = array ! x ! y

isPath :: Grid -> Position -> Bool
isPath grid position = tile `elem` [Path, SteepUp, SteepRight, SteepDown, SteepLeft]
  where
    tile = lookup grid position

isInGrid :: Grid -> Position -> Bool
isInGrid grid (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY
  where
    (minX, maxX) = bounds grid
    (minY, maxY) = bounds (grid ! 0)

neighbours :: Position -> [Position]
neighbours (x, y) = [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]

-- assumes the end position is always one tile left of the bottom right corner
getEndPosition :: Grid -> Position
getEndPosition grid = (snd . bounds $ grid, subtract 1 . snd . bounds $ grid ! 0)

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
