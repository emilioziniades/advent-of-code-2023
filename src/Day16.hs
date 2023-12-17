module Day16 (energizedSquares) where

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
    pure $ Set.size $ Set.fromList $ fst <$> Set.toList (followBeams grid Set.empty (Seq.singleton (Point 0 0, Right)))

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
