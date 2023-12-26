module Day12 (springArrangements, largeSpringArrangements) where

import Data.Bifunctor
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Util.Lists

type Cache = Map.Map (String, [Int]) Int

-- Part 1

springArrangements :: FilePath -> IO Int
springArrangements filename = do
    file <- readFile filename
    pure $ sum $ springArrangement <$> parseInput file

springArrangement :: (String, [Int]) -> Int
springArrangement (spring, springId) = length $ filter (== springId) $ getSpringId <$> allPossibleSprings spring

allPossibleSprings :: String -> [String]
allPossibleSprings "" = [""]
allPossibleSprings (s : ss) = do
    x <- possibleSpring s
    xs <- allPossibleSprings ss
    pure (x : xs)

possibleSpring :: Char -> [Char]
possibleSpring c
    | c == '?' = knownSprings
    | c `elem` knownSprings = [c]
    | otherwise = undefined
  where
    knownSprings = ['.', '#']

getSpringId :: [Char] -> [Int]
getSpringId spring = length <$> filter ("#" `isPrefixOf`) (group spring)

-- Part 2

largeSpringArrangements :: FilePath -> IO Int
largeSpringArrangements filename = do
    file <- readFile filename
    let input = isolateSpringSections . unfoldRow <$> parseInput file
    let (n, _cache) = springArrangements' Map.empty input
    pure n

springArrangements' :: Cache -> [(String, [Int])] -> (Int, Cache)
springArrangements' cache [] = (0, cache)
springArrangements' cache (s : ss) = first (+ n) $ springArrangements' newCache ss
  where
    subProblems = createSpringSubproblems s
    (n, newCache) = springArrangement' cache subProblems

springArrangement' :: Cache -> [(String, [Int])] -> (Int, Cache)
springArrangement' cache [] = (1, cache)
springArrangement' cache (s : ss) = first (* n) $ springArrangement' newCache ss
  where
    (n, newCache) = springArrangementCached cache s

springArrangementCached :: Cache -> (String, [Int]) -> (Int, Cache)
springArrangementCached cache spring = case Map.lookup spring cache of
    Nothing ->
        let
            n = countPossibleArrangements spring
         in
            (n, Map.insert spring n cache)
    Just n -> (n, cache)

unfoldRow :: (String, [Int]) -> (String, [Int])
unfoldRow (spring, springId) = (intercalate "?" $ replicate 5 spring, concat $ replicate 5 springId)

-- Steps
-- 1. Replace the ? surrounding # if there is only one option
-- 2. Split (String, [Int]) into multiple subproblems [(String, [Int])]
-- 3. For each (String, [Int]) subproblem, figure out how many ways
-- 4. multiply all the sub-ways together
-- 5. Go through the above and have a global cache

isolateSpringSections :: (String, [Int]) -> (String, [Int])
isolateSpringSections (spring, springId) = (sureThings, springId)
  where
    candidates = filter (isSpringAdjacent spring) [0 .. length spring - 1]
    sureThings = foldr (replaceSureThing springId) spring candidates

replaceSureThing :: [Int] -> Int -> String -> String
replaceSureThing springId i spring
    | not springPossible = unknownAsEmpty
    | not emptyPossible = unknownAsSpring
    | otherwise = spring
  where
    unknownAsEmpty = replaceIndex i spring '.'
    unknownAsSpring = replaceIndex i spring '#'
    springPossible = isPartialSpringPossible (unknownAsSpring, springId)
    emptyPossible = isPartialSpringPossible (unknownAsEmpty, springId)

isPartialSpringPossible :: (String, [Int]) -> Bool
isPartialSpringPossible (spring, springId) = all (`elem` springId) certainSpringId && maximum sketchySpringId <= maximum springId
  where
    certainSpringId = getCertainSpringId spring
    sketchySpringId = getSpringId spring

-- only gets IDs of springs surrounded by '.'
getCertainSpringId :: String -> [Int]
getCertainSpringId spring = fmap length $ filter (all (== '#')) $ groupBy areNotFullStops spring
  where
    areNotFullStops x y = x /= '.' && y /= '.'

replaceIndex :: Int -> String -> Char -> String
replaceIndex i spring c = take i spring <> pure c <> drop (i + 1) spring

isSpringAdjacent :: String -> Int -> Bool
isSpringAdjacent s i
    | not isQMark = False
    | prevI < 0 = nextIsSpring
    | nextI >= l = prevIsSpring
    | otherwise = prevIsSpring || nextIsSpring
  where
    l = length s
    prevI = i - 1
    nextI = i + 1
    isQMark = s !! i == '?'
    prevIsSpring = s !! prevI == '#'
    nextIsSpring = s !! nextI == '#'

createSpringSubproblems :: (String, [Int]) -> [(String, [Int])]
createSpringSubproblems ("", []) = []
createSpringSubproblems (_, []) = error "exhausted ids before spring"
createSpringSubproblems ("", _) = error "exhausted spring before ids"
createSpringSubproblems (spring, springId) = case firstSureThing of
    Nothing -> pure (spring, springId)
    Just i ->
        if not (null springsBeforeSureThing) && not (null nsBeforeSureThing)
            then
                [(springsBeforeSureThing, nsBeforeSureThing), (sureThing, [sureThingSize])]
                    <> createSpringSubproblems (springsAfterSureThing, nsAfterSureThing)
            else
                [(sureThing, [sureThingSize])]
                    <> createSpringSubproblems (springsAfterSureThing, nsAfterSureThing)
      where
        sureThing = groups !! i
        sureThingSize = length sureThing
        sureThingN = fromJust $ elemIndex sureThingSize springId
        springsBeforeSureThing = intercalate "." $ take i groups
        nsBeforeSureThing = take sureThingN springId
        springsAfterSureThing = intercalate "." $ drop (i + 1) groups
        nsAfterSureThing = drop (sureThingN + 1) springId
  where
    groups = filter (notElem '.') $ groupBy (\x y -> x /= '.' && y /= '.') spring
    firstSureThing = findIndex (all (== '#')) groups

countPossibleArrangements :: (String, [Int]) -> Int
countPossibleArrangements (spring, springId)
    | '?' `notElem` spring && isExactMatch = 1
    | '?' `notElem` spring && not isExactMatch = 0
    | springIdUntilQMark `isPrefixOf` springId = countPossibleArrangements (replaceIndex qMarkIndex spring '#', springId) + countPossibleArrangements (replaceIndex qMarkIndex spring '.', springId)
    | otherwise = 0
  where
    springUntilQMark = dropWhileEnd (/= '.') spring
    springIdUntilQMark = getSpringId springUntilQMark
    qMarkIndex = fromJust $ '?' `elemIndex` spring
    isExactMatch = getSpringId spring == springId

-- Input parsing

parseInput :: String -> [(String, [Int])]
parseInput file = fmap parseRow (lines file)
  where
    parseRow row =
        let
            (spring, springIdRaw) = splitOn2 ' ' row
            springId = read <$> splitOn ',' springIdRaw
         in
            (spring, springId)
