module Day12 (springArrangements) where

import Data.List
import Util.Lists

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

parseInput :: String -> [(String, [Int])]
parseInput file = fmap parseRow (lines file)
  where
    parseRow row =
        let
            (spring, springIdRaw) = splitOn2 ' ' row
            springId = read <$> splitOn ',' springIdRaw
         in
            (spring, springId)
