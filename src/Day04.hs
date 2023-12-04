module Day04 (scratchcardPoints) where

import Data.Set as Set
import Parse

data ScratchCard = ScratchCard {winningNumbers :: [Int], numbers :: [Int]}
    deriving (Show)

scratchcardPoints :: String -> IO Int
scratchcardPoints filename = do
    file <- readFile filename
    pure $ sum $ scoreScratchcard . parseScratchcard <$> lines file

parseScratchcard :: String -> ScratchCard
parseScratchcard line =
    let
        (_, allNs) = splitOn2 ':' line
        (winNs, ns) = splitOn2 '|' allNs
     in
        ScratchCard (read <$> words winNs) (read <$> words ns)

scoreScratchcard :: ScratchCard -> Int
scoreScratchcard (ScratchCard winNs ns) =
    if nOverlap == 0
        then 0
        else 2 ^ (nOverlap - 1)
  where
    nOverlap = Set.size $ Set.intersection (Set.fromList winNs) (Set.fromList ns)
