module Day02 (sumPossibleGameIds, sumCubePower) where

import Prelude hiding (round)

type Green = Int
type Red = Int
type Blue = Int

data Cubes = Cubes Red Green Blue
type Round = Cubes

data Game = Game {gameId :: Int, _rounds :: [Round]}

-- Part 1

sumPossibleGameIds :: String -> IO Int
sumPossibleGameIds filename = do
    file <- readFile filename
    let totals = Cubes 12 13 14
    return $ sum $ map gameId $ filter (isGamePossible totals) $ map parseGame $ lines file

isGamePossible :: Cubes -> Game -> Bool
isGamePossible bs (Game _ rs) = all (isRoundPossible bs) rs

isRoundPossible :: Cubes -> Round -> Bool
isRoundPossible (Cubes tR tG tB) (Cubes r g b) = r <= tR && g <= tG && b <= tB

-- Part 2

sumCubePower :: String -> IO Int
sumCubePower filename = do
    file <- readFile filename
    return $ sum $ map (cubePower . minimumViableCubes . parseGame) (lines file)

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
parseRounds rounds = map parseRound (splitOn ';' rounds)

parseRound :: String -> Round
parseRound round = foldr updateRound (Cubes 0 0 0) (splitOn ',' round)

updateRound :: String -> Round -> Round
updateRound round (Cubes r g b) = case words round of
    [n, "red"] -> Cubes (read n) g b
    [n, "green"] -> Cubes r (read n) b
    [n, "blue"] -> Cubes r g (read n)
    _ -> error ("badly structured round:" ++ show (words round))

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn char list
    | char `notElem` list = [list]
    | otherwise =
        let
            (a, b) = splitOn2 char list
         in
            if char `elem` b
                then a : splitOn char b
                else [a, b]

splitOn2 :: (Eq a) => a -> [a] -> ([a], [a])
splitOn2 char list = case span (/= char) list of
    (as, _ : bs) -> (as, bs)
    (as, []) -> (as, [])
