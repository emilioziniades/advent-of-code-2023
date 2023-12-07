module Day07 (totalWinnings, totalWinnings') where

import Data.Bifunctor
import Data.List
import Util.Lists

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Show, Enum)

newtype JokerCard = JokerCard {getCard :: Card}
    deriving (Eq, Show)

instance Ord JokerCard where
    JokerCard Jack <= JokerCard _ = True
    JokerCard _ <= JokerCard Jack = False
    JokerCard c1 <= JokerCard c2 = c1 <= c2

data HandType = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
    deriving (Eq, Ord, Show, Enum)

type Hand = [Card]
type Hand' = [JokerCard]

type Bid = Int

-- Part 1

totalWinnings :: FilePath -> IO Int
totalWinnings filename = do
    file <- readFile filename
    pure $ sum $ zipWith (*) [1 ..] $ snd <$> sortBy (compareHands scoreHand) (parseInput parseCard file)

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
    isLength n arr = length arr == n

-- Part 2

totalWinnings' :: FilePath -> IO Int
totalWinnings' filename = do
    file <- readFile filename
    pure $ sum $ zipWith (*) [1 ..] $ snd <$> sortBy (compareHands scoreHand') (parseInput (JokerCard . parseCard) file)

scoreHand' :: Hand' -> HandType
scoreHand' hand
    | JokerCard Jack `elem` hand = maximum $ scoreHand <$> expandJokers (fmap getCard (sort hand))
    | otherwise = scoreHand $ fmap getCard hand

expandJokers :: Hand -> [Hand]
expandJokers [c1, c2, c3, c4, c5] = do
    c1' <- expandCard c1
    c2' <- expandCard c2
    c3' <- expandCard c3
    c4' <- expandCard c4
    c5' <- expandCard c5
    pure [c1', c2', c3', c4', c5']
expandJokers _ = error "hands have five cards"

expandCard :: Card -> [Card]
expandCard Jack = [Two .. Ten] <> [Queen .. Ace]
expandCard c = [c]

-- Common to Part 1 and 2

-- see 'The ordering monoid' in http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids
compareHands :: (Ord a) => (a -> HandType) -> (a, Bid) -> (a, Bid) -> Ordering
compareHands handScorer (h1, _) (h2, _) = (handScorer h1 `compare` handScorer h2) <> (h1 `compare` h2)

-- Input Parsing

parseInput :: (Char -> a) -> String -> [([a], Bid)]
parseInput cardParser file = fmap parseLine (lines file)
  where
    parseLine line = bimap (fmap cardParser) read $ splitOn2 ' ' line

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
