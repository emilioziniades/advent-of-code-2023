{-# LANGUAGE TupleSections #-}

module Day04 (scratchcardPoints, scratchcardsTotal) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Parse

type CardId = Int
type Matches = Int
type Count = Int

type WinningNumbers = [Int]
type Numbers = [Int]

type ScratchcardMatches = Map.Map CardId Matches
type ScratchcardCounts = Map.Map CardId Count

data Scratchcard = Scratchcard CardId WinningNumbers Numbers
    deriving (Show)

-- Part 1

scratchcardPoints :: FilePath -> IO Int
scratchcardPoints filename = do
    file <- readFile filename
    pure $ sum $ scoreScratchcard . parseScratchcard <$> lines file

-- Part 2

scratchcardsTotal :: FilePath -> IO Int
scratchcardsTotal filename = do
    file <- readFile filename
    let scratchcards = parseScratchcard <$> lines file
    pure $ sum $ Map.elems $ foldCopies (makeMatchesCount scratchcards) (makeCounts scratchcards)

foldCopies :: ScratchcardMatches -> ScratchcardCounts -> ScratchcardCounts
foldCopies matches counts = foldl (foldCopy matches) counts [1 .. Map.size counts]

foldCopy :: ScratchcardMatches -> ScratchcardCounts -> CardId -> ScratchcardCounts
foldCopy matches counts cardId = foldl incrementCount counts [cardId + 1 .. cardId + cardMatches]
  where
    cardMatches = sum $ Map.lookup cardId matches
    cardCounts = sum $ Map.lookup cardId counts
    incrementCount cs cId = Map.update (\x -> Just $ x + cardCounts) cId cs

makeMatchesCount :: [Scratchcard] -> ScratchcardMatches
makeMatchesCount = foldr foldScratchcards Map.empty
  where
    foldScratchcards scratchcard@(Scratchcard cardId _ _) = Map.insert cardId (countMatchingNumbers scratchcard)

makeCounts :: [Scratchcard] -> ScratchcardCounts
makeCounts scratchcards = Map.fromList $ fmap (,1) [1 .. length scratchcards]

-- common to Parts 1 and 2

parseScratchcard :: String -> Scratchcard
parseScratchcard line =
    let
        (cId, allNs) = splitOn2 ':' line
        (winNs, ns) = splitOn2 '|' allNs
     in
        Scratchcard (read . last . words $ cId) (read <$> words winNs) (read <$> words ns)

scoreScratchcard :: Scratchcard -> Int
scoreScratchcard scratchcard =
    if matches == 0
        then 0
        else 2 ^ (matches - 1)
  where
    matches = countMatchingNumbers scratchcard

countMatchingNumbers :: Scratchcard -> Int
countMatchingNumbers (Scratchcard _ winningNs ns) = Set.size $ Set.intersection (Set.fromList winningNs) (Set.fromList ns)
