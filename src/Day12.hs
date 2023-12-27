module Day12 (springArrangements, largeSpringArrangements) where

import Data.List
import Data.MemoTrie (memo)
import Util.Lists

-- Part 1

springArrangements :: FilePath -> IO Int
springArrangements filename = do
    file <- readFile filename
    pure $ sum $ countWays <$> parseInput file

-- Part 2

largeSpringArrangements :: FilePath -> IO Int
largeSpringArrangements filename = do
    file <- readFile filename
    pure $ sum $ countWays . unfoldRow <$> parseInput file

-- Common to Part 1 and 2

countWays :: (String, [Int]) -> Int
countWays = memo countWays'
  where
    countWays' ("", []) = 1
    countWays' ("", _) = 0
    countWays' (l, [])
        | '#' `notElem` l = 1
        | otherwise = 0
    countWays' (line, runs) | length line < sum runs + length runs - 1 = 0
    countWays' ('.' : ls, rs) = countWays (ls, rs)
    countWays' ('#' : ls, r : rs)
        | '.' `elem` take (r - 1) ls = 0
        | isJustAnd (== '#') (ls !? (r - 1)) = 0
        | otherwise = countWays (drop r ls, rs)
    countWays' ('?' : ls, rs) = countWays ('#' : ls, rs) + countWays ('.' : ls, rs)
    countWays' (_, _) = undefined

-- Input parsing

parseInput :: String -> [(String, [Int])]
parseInput file = fmap parseRow (lines file)
  where
    parseRow row = (spring, springId)
      where
        (spring, springIdRaw) = splitOn2 ' ' row
        springId = read <$> splitOn ',' springIdRaw

unfoldRow :: (String, [Int]) -> (String, [Int])
unfoldRow (spring, springId) = (intercalate "?" $ replicate 5 spring, concat $ replicate 5 springId)

-- Utility functions

isJustAnd :: (a -> Bool) -> Maybe a -> Bool
isJustAnd _ Nothing = False
isJustAnd f (Just a) = f a

(!?) :: [a] -> Int -> Maybe a
xs !? i
    | i < length xs = Just (xs !! i)
    | otherwise = Nothing
