module Day02 (sumPossibleGameIds) where

import Prelude hiding (round)

type Green = Int
type Red = Int
type Blue = Int

data Blocks = Blocks Red Green Blue
type Round = Blocks

data Game = Game {gameId :: Int, _rounds :: [Round]}

sumPossibleGameIds :: String -> IO Int
sumPossibleGameIds filename = do
    file <- readFile filename
    let totals = Blocks 12 13 14
    return $ sum $ map gameId $ filter (isGamePossible totals) $ map parseGame $ lines file

isGamePossible :: Blocks -> Game -> Bool
isGamePossible bs (Game _ rs) = all (isRoundPossible bs) rs

isRoundPossible :: Blocks -> Round -> Bool
isRoundPossible (Blocks tR tG tB) (Blocks r g b) = r <= tR && g <= tG && b <= tB

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
parseRound round = foldr updateRound (Blocks 0 0 0) (splitOn ',' round)

updateRound :: String -> Round -> Round
updateRound round (Blocks r g b) = case words round of
    [n, "red"] -> Blocks (read n) g b
    [n, "green"] -> Blocks r (read n) b
    [n, "blue"] -> Blocks r g (read n)
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
