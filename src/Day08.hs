module Day08 (countSteps, countSimultaneousSteps) where

import Data.Char
import Data.List
import qualified Data.Map as Map

data Instruction = L | R deriving (Show, Read)

type Key = String
type Graph = Map.Map Key (String, String)

-- Part 1

countSteps :: FilePath -> IO Int
countSteps filename = do
    file <- readFile filename
    let (instrs, graph) = parseInput file
    let start = "AAA"
    pure $ findZZZ (cycle instrs) graph start 0

findZZZ :: [Instruction] -> Graph -> Key -> Int -> Int
findZZZ [] _ _ _ = error "instructions should cycle forever"
findZZZ (instr : instrs) graph key n
    | "Z" `isSuffixOf` key = n
    | otherwise = findZZZ instrs graph (step instr . unwrap $ Map.lookup key graph) n + 1
  where
    step i = case i of
        L -> fst
        R -> snd

-- Part 2

countSimultaneousSteps :: FilePath -> IO Int
countSimultaneousSteps filename = do
    file <- readFile filename
    let (instrs, graph) = parseInput file
    let allStarts = filter ("A" `isSuffixOf`) (Map.keys graph)
    print allStarts
    let allStepCounts = fmap (\k -> findZZZ (cycle instrs) graph k 0) allStarts
    pure $ lcmList allStepCounts

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

unwrap :: Maybe a -> a
unwrap (Just x) = x
unwrap Nothing = error "unwrapped nothing"

lcmList :: (Integral a) => [a] -> a
lcmList (n1 : n2 : ns) = lcmList (lcm n1 n2 : ns)
lcmList [n] = n
lcmList [] = error "an empty list has no lcm"
