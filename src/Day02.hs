module Day02 (sumPossibleGameIds, sumCubePower) where

import Parse
import Prelude hiding (round)

type Green = Int
type Red = Int
type Blue = Int

data Cubes = Cubes Red Green Blue
type Round = Cubes

data Game = Game {gameId :: Int, _gameRounds :: [Round]}

-- Part 1

sumPossibleGameIds :: FilePath -> IO Int
sumPossibleGameIds filename = do
    file <- readFile filename
    let totals = Cubes 12 13 14
    pure $ sum $ fmap gameId $ filter (isGamePossible totals) $ parseGame <$> lines file

isGamePossible :: Cubes -> Game -> Bool
isGamePossible cs (Game _ rs) = all (isRoundPossible cs) rs

isRoundPossible :: Cubes -> Round -> Bool
isRoundPossible (Cubes tR tG tB) (Cubes r g b) = r <= tR && g <= tG && b <= tB

-- Part 2

sumCubePower :: FilePath -> IO Int
sumCubePower filename = do
    file <- readFile filename
    pure $ sum $ fmap (cubePower . minimumViableCubes . parseGame) (lines file)

minimumViableCubes :: Game -> Cubes
minimumViableCubes (Game _ rounds) = foldr1 maxEachColour rounds

maxEachColour :: Cubes -> Cubes -> Cubes
maxEachColour (Cubes r1 g1 b1) (Cubes r2 g2 b2) = Cubes (max r1 r2) (max g1 g2) (max b1 b2)

cubePower :: Cubes -> Int
cubePower (Cubes r g b) = r * g * b

-- Parsing input

parseGame :: String -> Game
parseGame line =
    let
        (game, rounds) = splitOn2 ':' line
        gId = read . last . words $ game
     in
        Game gId (parseRounds rounds)

parseRounds :: String -> [Round]
parseRounds rounds = fmap parseRound (splitOn ';' rounds)

parseRound :: String -> Round
parseRound round = foldr updateRound (Cubes 0 0 0) (splitOn ',' round)

updateRound :: String -> Round -> Round
updateRound round (Cubes r g b) = case words round of
    [n, "red"] -> Cubes (read n) g b
    [n, "green"] -> Cubes r (read n) b
    [n, "blue"] -> Cubes r g (read n)
    _ -> error ("badly structured round:" <> show (words round))
