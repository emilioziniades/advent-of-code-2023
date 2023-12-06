module Day05 (lowestLocationNumber, lowestLocationNumberRanges) where

import Data.Char
import Parse

type Seed = Int

data SeedRange = SeedRange {seedRangeStart :: Int, seedRangeSize :: Int}
    deriving (Show)

type Almanac = [[Mapping]]

data Mapping = Mapping {sourceRangeStart :: Int, destinationRangeStart :: Int, rangeLength :: Int}
    deriving (Show, Eq, Ord)

-- Part 1

lowestLocationNumber :: FilePath -> IO Int
lowestLocationNumber filename = do
    file <- readFile filename
    let (almanac, seeds) = parseInput parseSeeds file
    pure $ minimum $ fmap (locationNumber almanac) seeds

locationNumber :: Almanac -> Seed -> Int
locationNumber mappings seed = foldl translateNumber seed mappings

translateNumber :: Int -> [Mapping] -> Int
translateNumber n [] = n
translateNumber n ((Mapping srcStart dstStart rangeSize) : ms) =
    if n >= srcStart && n < (srcStart + rangeSize)
        then (n - srcStart) + dstStart
        else translateNumber n ms

-- Part 2

lowestLocationNumberRanges :: FilePath -> IO Int
lowestLocationNumberRanges filename = do
    file <- readFile filename
    let (almanac, seedRanges) = parseInput parseSeedRanges file
    pure $ minimum $ seedRangeStart <$> locationNumberRanges almanac seedRanges

locationNumberRanges :: Almanac -> [SeedRange] -> [SeedRange]
locationNumberRanges mappings ranges = foldl (\rs m -> concatMap (translateRange m m) rs) ranges mappings

translateRange :: [Mapping] -> [Mapping] -> SeedRange -> [SeedRange]
translateRange allMs (m : ms) s
    -- easiest case - the seed range falls entirely within one mapping range
    | seedStartInMapping && seedEndInMapping = [SeedRange (seedStart - srcStart + dstStart) seedRange]
    -- seed range starts outside of mapping but ends inside it
    | not seedStartInMapping && seedEndInMapping =
        translateRange allMs allMs (SeedRange seedStart (srcStart - seedStart))
            <> [SeedRange dstStart (seedEnd - srcStart)]
    -- seed range starts inside of mapping but ends outside it
    | seedStartInMapping && not seedEndInMapping =
        [SeedRange (seedStart - srcStart + dstStart) (srcEnd - seedStart)]
            <> translateRange allMs allMs (SeedRange srcEnd (seedEnd - srcEnd))
    -- seed range starts and ends outside mapping, but middle chunk is inside it
    | seedWhollyContainsMapping =
        translateRange allMs allMs (SeedRange seedStart (srcStart - seedStart))
            <> [SeedRange dstStart range]
            <> translateRange allMs allMs (SeedRange srcEnd (seedEnd - srcEnd))
    -- fallback - there is absolutely no overlap between mapping and range. Try next mapping
    | otherwise = translateRange allMs ms s
  where
    (Mapping srcStart dstStart range) = m
    (SeedRange seedStart seedRange) = s
    -- ends
    srcEnd = srcStart + range
    seedEnd = seedStart + seedRange
    seedStartInMapping = seedStart >= srcStart && seedStart < srcEnd
    seedEndInMapping = seedEnd > srcStart && seedEnd <= srcEnd
    seedWhollyContainsMapping = seedStart < srcStart && seedEnd > srcEnd
translateRange _ [] s = [s]

{-
Think about it like this:

there are two three cases of seed and mapping range sizes

equal:
seed       ----
mapping    ----

seed-larger:
seed       ------
mapping     ----

seed-smaller:
seed        ----
mapping    ------

take the 'equal' scenario. there are various possible overlaps

seed     ----
mapping        ----

seed     ----
mapping    ----

seed       ----
mapping    ----

seed         ----
mapping    ----

seed             ----
mapping    ----

take the 'seed-larger' scenario. there are various possible overlaps

seed       ------
mapping            ----

seed       ------
mapping        ----

seed       ------
mapping     ----

seed         ------
mapping     ----

seed             ------
mapping     ----

-}

-- idea: split ranges once, and then process them once

-- too tired but here's the plan:
-- you need to handle the ranges as ranges, not individual seeds as there are too many
-- best case: the seed range falls entirely within a single mapping. Easy. Map the range.
-- worst case: SOME of the seed range falls within this range. So, you have to split the range
-- up into smaller ranges such that each sub-range can be mapped. Split up the range on the boundaries
-- of the current mapping, recursively, until you are left with only ranges that can be entirely mapped.

-- Input parsing

parseInput :: (String -> a) -> String -> (Almanac, a)
parseInput seedsParser file = ((fmap . fmap) parseMappingRow mappingsRows, seedsParser seedsRow)
  where
    rows = lines file
    seedsRow = dropWhile (not . isNumber) $ head rows
    mappingsRows = filter (not . null) $ splitOn "" $ filter (not . any isLetter) $ tail rows

parseSeeds :: String -> [Seed]
parseSeeds seedsRow = read <$> words seedsRow

parseSeedRanges :: String -> [SeedRange]
parseSeedRanges seedsRow = parseSeedRange <$> windowsN 2 (words seedsRow)

parseSeedRange :: [String] -> SeedRange
parseSeedRange [x, y] = SeedRange (read x) (read y)
parseSeedRange _ = error "malformed seed range"

parseMappingRow :: String -> Mapping
parseMappingRow mappingRow =
    case words mappingRow of
        [x, y, z] -> Mapping (read y) (read x) (read z)
        _ -> error $ "malformed mapping row " <> mappingRow

-- Utility functions

windowsN :: Int -> [a] -> [[a]]
windowsN _ [] = []
windowsN n xs = take n xs : windowsN n (drop n xs)
