module Day16 (energizedSquares) where

import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import Util.Grid
import Prelude hiding (Left, Right)

data Direction = Right | Down | Left | Up
    deriving (Eq, Ord, Show)

energizedSquares :: FilePath -> IO Int
energizedSquares filename = do
    file <- readFile filename
    let grid = gridMap (lines file)
    pure $ Set.size $ Set.fromList $ fst <$> followBeams grid Set.empty (Point 0 0, Right)

followBeams :: Map.Map Point Char -> Set.Set (Point, Direction) -> (Point, Direction) -> [(Point, Direction)]
followBeams grid visited (point, direction) = (point, direction) : concatMap (followBeams grid newVisited) nextPoints
  where
    cell = fromJust $ Map.lookup point grid
    nextPoints = filter (`Set.notMember` visited) $ filter (flip Map.member grid . fst) $ getNextPoints point direction cell
    newVisited = Set.insert (point, direction) visited

getNextPoints :: Point -> Direction -> Char -> [(Point, Direction)]
-- getNextPoints pt direction char | traceShow (pt, direction, char) False = undefined
-- getNextPoints _ _ _ = []
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
