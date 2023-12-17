module Day16 (energizedSquares, maximumEnergy) where

import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Util.Grid
import Prelude hiding (Left, Right)

data Direction = Right | Down | Left | Up
    deriving (Eq, Ord, Show)

-- Part 1

energizedSquares :: FilePath -> IO Int
energizedSquares filename = do
    file <- readFile filename
    let grid = gridMap (lines file)
    pure $ countEnergizedSqures grid (Point 0 0, Right)

-- Part 2

maximumEnergy :: FilePath -> IO Int
maximumEnergy filename = do
    file <- readFile filename
    let rawGrid = lines file
    let grid = gridMap rawGrid
    pure $ maximum $ fmap (countEnergizedSqures grid) (getAllStarts rawGrid)

getAllStarts :: [String] -> [(Point, Direction)]
getAllStarts rawGrid = topStarts <> leftStarts <> rightStarts <> bottomStarts
  where
    cols = length (head rawGrid) - 1
    rows = length rawGrid - 1
    topStarts = fmap (\n -> (Point 0 n, Down)) [0 .. cols]
    leftStarts = fmap (\n -> (Point n 0, Right)) [0 .. rows]
    rightStarts = fmap (\n -> (Point n cols, Left)) [0 .. rows]
    bottomStarts = fmap (\n -> (Point rows n, Up)) [0 .. cols]

-- Common to Part 1 and Part 2

countEnergizedSqures :: Map.Map Point Char -> (Point, Direction) -> Int
countEnergizedSqures grid start = Set.size $ Set.fromList $ fst <$> Set.toList (followBeams grid Set.empty (Seq.singleton start))

followBeams :: Map.Map Point Char -> Set.Set (Point, Direction) -> Seq.Seq (Point, Direction) -> Set.Set (Point, Direction)
followBeams _ visited Seq.Empty = visited
followBeams grid visited (q@(pt, d) :<| qs) = followBeams grid newVisited queue
  where
    newVisited = Set.insert q visited
    cell = fromJust $ Map.lookup pt grid
    nextPoints = filter (`Set.notMember` visited) $ filter (flip Map.member grid . fst) $ getNextPoints pt d cell
    queue = foldl (|>) qs nextPoints

getNextPoints :: Point -> Direction -> Char -> [(Point, Direction)]
getNextPoints (Point x y) direction char =
    let
        right = (Point x (y + 1), Right)
        left = (Point x (y - 1), Left)
        down = (Point (x + 1) y, Down)
        up = (Point (x - 1) y, Up)
     in
        case (direction, char) of
            -- horizontal mirror
            (Right, '-') -> pure right
            (Down, '-') -> [left, right]
            (Left, '-') -> pure left
            (Up, '-') -> [left, right]
            -- vertical mirror
            (Right, '|') -> [up, down]
            (Down, '|') -> pure down
            (Left, '|') -> [up, down]
            (Up, '|') -> pure up
            -- left-diagonal mirror
            (Right, '\\') -> pure down
            (Down, '\\') -> pure right
            (Left, '\\') -> pure up
            (Up, '\\') -> pure left
            -- right-diagonal mirror
            (Right, '/') -> pure up
            (Down, '/') -> pure left
            (Left, '/') -> pure down
            (Up, '/') -> pure right
            -- empty space
            (Right, '.') -> pure right
            (Down, '.') -> pure down
            (Left, '.') -> pure left
            (Up, '.') -> pure up
            (_, _) -> error "did not consider this"
