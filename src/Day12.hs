module Day12 (springArrangements) where

import Data.Char
import Data.List
import Util.Lists

-- too low: 7672

springArrangements :: FilePath -> IO Int
springArrangements filename = do
    file <- readFile filename
    -- print $ springArrangement <$> parseInput file
    pure $ sum $ springArrangement <$> parseInput file

springArrangement :: (String, [Int]) -> Int
-- springArrangement (spring, _) | traceShow spring False = undefined
-- springArrangement (_, springId) | traceShow springId False = undefined
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
-- getSpringId spring = trace (spring <> show theId) theId
getSpringId spring = theId
  where
    theId = length <$> filter ("#" `isPrefixOf`) (group spring)

parseInput :: String -> [(String, [Int])]
parseInput file = fmap parseRow (lines file)
  where
    parseRow row =
        let
            (spring, springIdRaw) = splitOn2 ' ' row
            springId = digitToInt <$> filter isNumber springIdRaw
         in
            (spring, springId)
