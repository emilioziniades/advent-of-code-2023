module Day19 (addAcceptedRatingNumbers, countRatingCombinations) where

import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Util.Lists (splitOn, splitOn2)

type RuleSet = Map.Map String [Rule]

data WorkflowLabel = Label String | Accept | Reject
    deriving (Show)

data RatingCategory = X | M | A | S
    deriving (Show)

data Comparison = GreaterThan | LessThan
    deriving (Show)

data Rule = Conditional RatingCategory Comparison Int WorkflowLabel | Direct WorkflowLabel
    deriving (Show)

data PartRating = PartRating
    { getX :: Int
    , getM :: Int
    , getA :: Int
    , getS :: Int
    }
    deriving (Show)

data PartRatingRange = PartRatingRange
    { getXRange :: (Int, Int)
    , getMRange :: (Int, Int)
    , getARange :: (Int, Int)
    , getSRange :: (Int, Int)
    }
    deriving (Show)

-- Part 1

addAcceptedRatingNumbers :: FilePath -> IO Int
addAcceptedRatingNumbers filename = do
    file <- readFile filename
    let (ruleSet, ratings) = parseInput file
    pure $ sum $ getTotal <$> filter (isAccepted ruleSet (Label "in")) ratings

getTotal :: PartRating -> Int
getTotal (PartRating x m a s) = x + m + a + s

isAccepted :: RuleSet -> WorkflowLabel -> PartRating -> Bool
isAccepted _ Accept _ = True
isAccepted _ Reject _ = False
isAccepted ruleSet (Label label) rating = isAccepted ruleSet nextLabel rating
  where
    rule = fromJust (Map.lookup label ruleSet)
    nextLabel = processRating rule rating

processRating :: [Rule] -> PartRating -> WorkflowLabel
processRating [] _ = error "exhausted rules without identifying next label"
processRating (Direct workflowLabel : _) _ = workflowLabel
processRating (Conditional category comparison n workflowLabel : rules) rating =
    if getComparison comparison (getRating category rating) n
        then workflowLabel
        else processRating rules rating

getComparison :: Comparison -> (Int -> Int -> Bool)
getComparison GreaterThan = (>)
getComparison LessThan = (<)

getRating :: RatingCategory -> PartRating -> Int
getRating X rating = getX rating
getRating M rating = getM rating
getRating A rating = getA rating
getRating S rating = getS rating

-- Part 2

countRatingCombinations :: FilePath -> IO Int
countRatingCombinations filename = do
    file <- readFile filename
    let (ruleSet, _) = parseInput file
    let initialRange = PartRatingRange (1, 4000) (1, 4000) (1, 4000) (1, 4000)
    let initialLabel = Label "in"
    pure $ sum $ getRangeCount <$> findAcceptableRanges ruleSet [(initialRange, initialLabel)]

findAcceptableRanges :: RuleSet -> [(PartRatingRange, WorkflowLabel)] -> [PartRatingRange]
findAcceptableRanges _ [] = []
findAcceptableRanges ruleSet ((ratingRange, Accept) : rest) = [ratingRange] <> findAcceptableRanges ruleSet rest
findAcceptableRanges ruleSet ((_, Reject) : rest) = findAcceptableRanges ruleSet rest
findAcceptableRanges ruleSet ((ratingRange, Label label) : rest) = findAcceptableRanges ruleSet (newRanges <> rest)
  where
    rule = fromJust (Map.lookup label ruleSet)
    newRanges = processRange rule ratingRange

processRange :: [Rule] -> PartRatingRange -> [(PartRatingRange, WorkflowLabel)]
processRange [] _ = error "exhausted rules without identifying next label"
processRange (Direct workflowLabel : _) ratingRange = [(ratingRange, workflowLabel)]
processRange rules@(Conditional category comparison n workflowLabel : rs) ratingRange
    | e > n && s < n = processRange rules lowerRange <> processRange rules upperRange
    | compareFn s n && compareFn e n = [(ratingRange, workflowLabel)]
    | otherwise = processRange rs ratingRange
  where
    compareFn = getComparison comparison
    (s, e) = getRange category ratingRange
    (lower, upper) = case comparison of
        GreaterThan -> ((s, n), (n + 1, e))
        LessThan -> ((s, n - 1), (n, e))
    lowerRange = updateRange category lower ratingRange
    upperRange = updateRange category upper ratingRange

getRange :: RatingCategory -> PartRatingRange -> (Int, Int)
getRange category rating = case category of
    X -> getXRange rating
    M -> getMRange rating
    A -> getARange rating
    S -> getSRange rating

updateRange :: RatingCategory -> (Int, Int) -> PartRatingRange -> PartRatingRange
updateRange category (s, e) (PartRatingRange xRange mRange aRange sRange) = case category of
    X -> PartRatingRange (s, e) mRange aRange sRange
    M -> PartRatingRange xRange (s, e) aRange sRange
    A -> PartRatingRange xRange mRange (s, e) sRange
    S -> PartRatingRange xRange mRange aRange (s, e)

getRangeCount :: PartRatingRange -> Int
getRangeCount
    ( PartRatingRange
            (x1, x2)
            (m1, m2)
            (a1, a2)
            (s1, s2)
        ) =
        (x2 - x1 + 1)
            * (m2 - m1 + 1)
            * (a2 - a1 + 1)
            * (s2 - s1 + 1)

-- Input parsing

parseInput :: String -> (RuleSet, [PartRating])
parseInput file = (Map.fromList $ parseRules <$> rawRules, parseRating <$> rawRatings)
  where
    (rawRules, rawRatings) = splitOn2 "" (lines file)

parseRating :: String -> PartRating
parseRating line = makeRating ratings
  where
    ratings = read . filter isNumber <$> splitOn ',' line
    makeRating [x, m, a, s] = PartRating x m a s
    makeRating _ = error "malformed rating"

parseRules :: String -> (String, [Rule])
parseRules line = (label, parseRule . groupBy areAlphaNum <$> allRules)
  where
    (label, rules) = span isAlpha line
    allRules = splitOn ',' (filter (not . isCurly) rules)
    isCurly c = c `elem` "{}"
    areAlphaNum c1 c2 = isAlphaNum c1 && isAlphaNum c2

parseRule :: [String] -> Rule
parseRule [x] = Direct (parseLabel x)
parseRule [x, "<", y, ":", z] = Conditional (parseRatingCategory x) LessThan (read y) (parseLabel z)
parseRule [x, ">", y, ":", z] = Conditional (parseRatingCategory x) GreaterThan (read y) (parseLabel z)
parseRule x = error (concat x <> ": not a valid workflow rule")

parseLabel :: String -> WorkflowLabel
parseLabel "A" = Accept
parseLabel "R" = Reject
parseLabel x = Label x

parseRatingCategory :: String -> RatingCategory
parseRatingCategory x = case x of
    "x" -> X
    "m" -> M
    "a" -> A
    "s" -> S
    _ -> error (x <> ": not a valid rating category")
