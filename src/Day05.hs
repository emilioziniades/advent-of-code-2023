module Day05 (lowestLocationNumber, lowestLocationNumberRanges) where

import Data.Char
import Parse (splitOn)

type Seed = Int

data SeedRange = SeedRange Int Int deriving (Show)

type Almanac = [[Mapping]]

data Mapping = Mapping
    { destinationRangeStart :: Int
    , sourceRangeStart :: Int
    , rangeLength :: Int
    }
    deriving (Show)

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
translateNumber n ((Mapping dstStart srcStart rangeSize) : ms) =
    if n >= srcStart && n < (srcStart + rangeSize)
        then (n - srcStart) + dstStart
        else translateNumber n ms

-- Part 2

lowestLocationNumberRanges :: FilePath -> IO Int
lowestLocationNumberRanges filename = do
    file <- readFile filename
    let (almanac, seedRanges) = parseInput parseSeedRanges file
    let x = head seedRanges
    print seedRanges
    -- print $ locationNumberRanges almanac seedRanges
    pure 0

locationNumberRanges :: Almanac -> [SeedRange] -> [SeedRange]
locationNumberRanges mappings ranges = foldl (\rs m -> concatMap (translateRange m) ranges) ranges mappings

-- note: may have to sort mappings first

translateRange :: [Mapping] -> SeedRange -> [SeedRange]
translateRange mappings@((Mapping dstStart srcStart rangeSize) : ms) seedRange@(SeedRange seedRangeStart seedRangeSize)
    -- easiest case - the seed range falls entirely within one mapping range
    | seedRangeStart >= srcStart && seedRangeEnd < srcEnd = [SeedRange newRangeStart seedRangeSize]
    -- seed range starts outside of mapping but ends inside it
    | seedRangeStart < srcStart && seedRangeEnd < srcEnd = translateRange mappings preSeedRange <> [SeedRange dstStart (seedRangeEnd - srcStart)]
    -- seed range starts inside of mapping but ends outside it
    | seedRangeStart >= srcStart && seedRangeEnd >= srcEnd = SeedRange newRangeStart (srcEnd - seedRangeStart) : translateRange mappings postSeedRange
    -- seed range starts and ends outside mapping, but middle chunk is inside it
    | seedRangeStart < srcStart && seedRangeEnd >= srcEnd = translateRange mappings preSeedRange <> [SeedRange newRangeStart rangeSize] <> translateRange mappings postSeedRange
    -- fallback - there is absolutely no overlap between mapping and range. Try next mapping
    | otherwise = translateRange ms seedRange
  where
    srcEnd = srcStart + rangeSize
    seedRangeEnd = seedRangeStart + seedRangeSize
    newRangeStart = (seedRangeStart - srcStart) + dstStart
    preSeedRange = SeedRange seedRangeStart (srcStart - seedRangeStart)
    postSeedRange = SeedRange srcEnd (seedRangeEnd - srcEnd)
translateRange [] seedRange = [seedRange]

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
        [x, y, z] -> Mapping (read x) (read y) (read z)
        _ -> error $ "malformed mapping row " <> mappingRow

-- Utility functions
windowsN :: Int -> [a] -> [[a]]
windowsN _ [] = []
windowsN n xs = take n xs : windowsN n (drop n xs)
