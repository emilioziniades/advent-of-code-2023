module Day07 (totalWinnings) where

import Data.Bifunctor
import Data.List
import Util.Lists

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Show)

data HandType = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
    deriving (Eq, Ord, Show)

type Hand = [Card]

type Bid = Int

-- Part 1

totalWinnings :: FilePath -> IO Int
totalWinnings filename = do
    file <- readFile filename
    pure $ sum $ zipWith (*) [1 ..] $ snd <$> sortBy compareHands (parseInput file)

-- see 'The ordering monoid' in http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids
compareHands :: (Hand, Bid) -> (Hand, Bid) -> Ordering
compareHands (h1, _) (h2, _) = (scoreHand h1 `compare` scoreHand h2) <> (h1 `compare` h2)

scoreHand :: Hand -> HandType
scoreHand hand
    | any (isLength 5) groups = FiveKind
    | any (isLength 4) groups = FourKind
    | any (isLength 3) groups && any (isLength 2) groups = FullHouse
    | any (isLength 3) groups = ThreeKind
    | length (filter (isLength 2) groups) == 2 = TwoPair
    | any (isLength 2) groups = OnePair
    | otherwise = HighCard
  where
    groups = group $ sort hand

isLength :: Int -> [a] -> Bool
isLength n arr = length arr == n

-- Input Parsing

parseInput :: String -> [(Hand, Bid)]
parseInput file = fmap parseLine (lines file)
  where
    parseLine line = bimap (fmap parseCard) read $ splitOn2 ' ' line

parseCard :: Char -> Card
parseCard c = case c of
    '2' -> Two
    '3' -> Three
    '4' -> Four
    '5' -> Five
    '6' -> Six
    '7' -> Seven
    '8' -> Eight
    '9' -> Nine
    'T' -> Ten
    'J' -> Jack
    'Q' -> Queen
    'K' -> King
    'A' -> Ace
    _ -> error (c : ": not a valid card ")
