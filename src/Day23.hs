module Day23 (findLongestHike) where

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

-- Part 1
--
findLongestHike :: String -> Int
findLongestHike file = longestHike grid (getEndPosition grid) (State (Position 0 1) Set.empty)
  where
    grid = parseInput file

longestHike :: Grid -> Position -> State -> Int
longestHike grid endPosition (State current@(Position x y) visited)
    | current == endPosition = 0
    | null nextNeighbours = 0 -- TODO: this is pretty gross, you'd rather not return anything
    | otherwise = (+ 1) $ maximum $ fmap (\s -> longestHike grid endPosition (State s nextVisited)) nextNeighbours
  where
    tile = lookup grid current
    nextNeighbours = filter (isPath grid) $ filter (`Set.notMember` visited) $ filter (isInGrid grid) $ case tile of
        SteepUp -> [Position (x - 1) y]
        SteepRight -> [Position x (y + 1)]
        SteepDown -> [Position (x + 1) y]
        SteepLeft -> [Position x (y - 1)]
        Path -> neighbours current
        Forest -> error "current position can never be a forest"
    nextVisited = Set.insert current visited

-- General grid logic

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

lookup :: Grid -> Position -> Tile
lookup array (Position x y) = array ! x ! y
