module Day08 (countSteps) where

import Data.Char
import qualified Data.Map as Map

data Instruction = L | R deriving (Show, Read)

type Key = String
type Graph = Map.Map Key (String, String)

-- Part 1

countSteps :: FilePath -> IO Int
countSteps filename = do
    file <- readFile filename
    let (instr, graph) = parseInput file
    pure $ findZZZ (cycle instr) graph "AAA" 0

findZZZ :: [Instruction] -> Graph -> Key -> Int -> Int
findZZZ _ _ "ZZZ" n = n
findZZZ [] _ _ _ = error "instructions should cycle forever"
findZZZ (instr : instrs) graph key n = findZZZ instrs graph (step instr . unwrap $ Map.lookup key graph) n + 1
  where
    step i = case i of
        L -> fst
        R -> snd

-- Input parsing

parseInput :: String -> ([Instruction], Graph)
parseInput file =
    let
        fileLines = filter (not . null) $ lines $ filter isLetterOrSpace file
        instrLines = head fileLines
        mapLines = tail fileLines
        getKeyValue row = case words row of
            [x, y, z] -> (x, (y, z))
            _ -> error (row <> ": not a network work")
     in
        (read . pure <$> instrLines, Map.fromList $ getKeyValue <$> mapLines)

-- Utility

isLetterOrSpace :: Char -> Bool
isLetterOrSpace c = isLetter c || isSpace c

unwrap :: Maybe a -> a
unwrap (Just x) = x
unwrap Nothing = error "unwrapped nothing"
