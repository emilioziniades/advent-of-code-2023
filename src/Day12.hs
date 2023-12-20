module Day12 (springArrangements, largeSpringArrangements) where

import Data.List
import Debug.Trace
import Util.Lists

-- Part 1

springArrangements :: FilePath -> IO Int
springArrangements filename = do
    file <- readFile filename
    pure $ sum $ springArrangement <$> parseInput file

springArrangement :: (String, [Int]) -> Int
springArrangement (spring, springId) = length $ filter (== springId) $ getSpringId <$> allPossibleSprings spring

allPossibleSprings :: String -> [String]
allPossibleSprings "" = [""]
allPossibleSprings (s : ss) = do
    x <- possibleSpring s
    xs <- allPossibleSprings ss
    pure (x : xs)

possibleSpring :: Char -> [Char]
possibleSpring c
    | c == '?' = knownSprings
    | c `elem` knownSprings = [c]
    | otherwise = undefined
  where
    knownSprings = ['.', '#']

getSpringId :: [Char] -> [Int]
getSpringId spring = length <$> filter ("#" `isPrefixOf`) (group spring)

-- Part 2

largeSpringArrangements :: FilePath -> IO Int
largeSpringArrangements filename = do
    file <- readFile filename
    print ""
    mapM_ print $ unfoldRow <$> parseInput file
    pure 0

unfoldRow :: (String, [Int]) -> (String, [Int])
unfoldRow (spring, springId) = (intercalate "?" $ replicate 5 spring, concat $ replicate 5 springId)

-- Steps
-- 1. Replace the ? surrounding # if there is only one option
-- 2. Split (String, [Int]) into multiple subproblems [(String, [Int])]
-- 3. For each (String, [Int]) subproblem, figure out how many ways
-- 4. multiply all the sub-ways together
-- 5. Go through the above and have a global cache

isolateSpringSections :: (String, [Int]) -> (String, [Int])
isolateSpringSections (spring, springId) = (spring, springId)
  where
    candidates = []

-- Input parsing

parseInput :: String -> [(String, [Int])]
parseInput file = fmap parseRow (lines file)
  where
    parseRow row =
        let
            (spring, springIdRaw) = splitOn2 ' ' row
            springId = read <$> splitOn ',' springIdRaw
         in
            (spring, springId)
