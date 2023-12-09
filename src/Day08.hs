module Day08 (countSteps, countGhostSteps) where

import Data.Char
import Data.List
import qualified Data.Map as Map
import Util.Maths
import Util.Maybe

data Instruction = L | R deriving (Show, Read)

type Key = String
type Graph = Map.Map Key (String, String)

-- Part 1

countSteps :: FilePath -> IO Int
countSteps filename = do
    file <- readFile filename
    let (instructions, graph) = parseInput file
    let start = "AAA"
    pure $ findEnd (cycle instructions) graph 0 start

-- Part 2

countGhostSteps :: FilePath -> IO Int
countGhostSteps filename = do
    file <- readFile filename
    let (instructions, graph) = parseInput file
    let allStarts = filter ("A" `isSuffixOf`) (Map.keys graph)
    pure $ lcmList $ fmap (findEnd (cycle instructions) graph 0) allStarts

-- Common to Part 1 and 2

findEnd :: [Instruction] -> Graph -> Int -> Key -> Int
findEnd [] _ _ _ = error "instructions should cycle forever"
findEnd (instruction : instructions) graph n key
    | "Z" `isSuffixOf` key = n
    | otherwise = findEnd instructions graph (n + 1) (step instruction . unwrap $ Map.lookup key graph)
  where
    step i = case i of
        L -> fst
        R -> snd

-- Input parsing

parseInput :: String -> ([Instruction], Graph)
parseInput file =
    let
        fileLines = filter (not . null) $ lines $ filter isAlphaNumOrSpace file
        instrLines = head fileLines
        mapLines = tail fileLines
        getKeyValue row = case words row of
            [x, y, z] -> (x, (y, z))
            _ -> error (row <> ": not a network work")
     in
        (read . pure <$> instrLines, Map.fromList $ getKeyValue <$> mapLines)

-- Utility

isAlphaNumOrSpace :: Char -> Bool
isAlphaNumOrSpace c = isAlphaNum c || isSpace c
