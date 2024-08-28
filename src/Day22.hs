module Day22 (countDisintegratable) where

import Data.List
import Data.Ord
import qualified Data.Set as Set
import Util.Lists (enumerate, splitOn, splitOn2)

data Cube = Cube {getStart :: Point, getEnd :: Point, getId :: Int}
    deriving (Show, Eq, Ord)

data Point = Point {getX :: Int, getY :: Int, getZ :: Int}
    deriving (Show, Eq)

-- compare points primarily based on their position on the z-axis
instance Ord Point where
    compare a b = mconcat [comparing getZ a b, comparing getY a b, comparing getX a b]

countDisintegratable :: FilePath -> IO Int
countDisintegratable filename = do
    file <- readFile filename
    mapM_ print (lines file)
    let cubes = parseCube <$> (enumerate . lines) file
    let cubesSet = Set.fromList $ concatMap getPoints cubes
    let droppedCubes = dropCubes (sort cubes) cubesSet
    print (sum $ area <$> cubes)
    print (Set.size cubesSet)
    mapM_ print droppedCubes

    pure 0

dropCubes :: [Cube] -> Set.Set Point -> [Cube]
dropCubes [] _ = []
dropCubes (c : cs) set = droppedC : dropCubes cs newSet
  where
    setWithoutC = foldr Set.delete set (getPoints c)
    droppedC = dropCube c setWithoutC
    newSet = foldr Set.insert setWithoutC (getPoints droppedC)

dropCube :: Cube -> Set.Set Point -> Cube
dropCube cube set
    -- reached floor
    | (getZ . getStart) cube == 1 || (getZ . getEnd) cube == 1 = cube
    -- can't drop any futher
    | any (`Set.member` set) (getPoints cube') = cube
    -- keep dropping
    | otherwise = dropCube cube' set
  where
    cube' = fall cube

-- Input parsing

parseCube :: (String, Int) -> Cube
parseCube (line, n) = Cube (parsePoint start) (parsePoint end) n
  where
    (start, end) = splitOn2 '~' line
    parsePoint s = case read <$> splitOn ',' s of
        [x, y, z] -> Point x y z
        _ -> error "malformed point string"

-- Utility functions

area :: Cube -> Int
area (Cube (Point xs ys zs) (Point xe ye ze) _) = (xe - xs + 1) * (ye - ys + 1) * (ze - zs + 1)

getPoints :: Cube -> [Point]
getPoints (Cube (Point xs ys zs) (Point xe ye ze) _) = [Point x y z | x <- [xs .. xe], y <- [ys .. ye], z <- [zs .. ze]]

fall :: Cube -> Cube
fall (Cube (Point xs ys zs) (Point xe ye ze) n) = Cube (Point xs ys (zs - 1)) (Point xe ye (ze - 1)) n
