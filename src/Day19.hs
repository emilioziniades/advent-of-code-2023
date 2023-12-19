module Day19 (addAcceptedRatingNumbers) where

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

data PartRating = PartRating {getX :: Int, getM :: Int, getA :: Int, getS :: Int}
    deriving (Show)

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
