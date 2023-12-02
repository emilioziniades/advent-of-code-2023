module Day02 (sumPossibleGameIds, sumCubePower) where

import Prelude hiding (round)

type Green = Int
type Red = Int
type Blue = Int

data Cubes = Cubes Red Green Blue
type Round = Cubes

data Game = Game {gameId :: Int, _gameRounds :: [Round]}

-- Part 1

sumPossibleGameIds :: String -> IO Int
sumPossibleGameIds filename = do
    file <- readFile filename
    let totals = Cubes 12 13 14
    return $ sum $ map gameId $ filter (isGamePossible totals) $ map parseGame $ lines file

isGamePossible :: Cubes -> Game -> Bool
isGamePossible cs (Game _ rs) = all (isRoundPossible cs) rs

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
splitOn e list
    | e `notElem` list = [list]
    | e `elem` l2 = l1 : splitOn e l2
    | otherwise = [l1, l2]
  where
    (l1, l2) = splitOn2 e list

splitOn2 :: (Eq a) => a -> [a] -> ([a], [a])
splitOn2 e list = case span (/= e) list of
    (as, _ : bs) -> (as, bs)
    (as, []) -> (as, [])
