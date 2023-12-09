module Day05 (lowestLocationNumber, lowestLocationNumberRanges) where

import Data.Char
import Data.List
import Util.Lists

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
locationNumberRanges mappings ranges = foldl (\rs ms -> concatMap (translateRange ms) rs) ranges (fmap sort mappings)

translateRange :: [Mapping] -> SeedRange -> [SeedRange]
translateRange (Mapping srcStart dstStart range : ms) s@(SeedRange seedStart seedRange)
    -- easiest case - the seed range falls entirely within one mapping range
    | seedStartInMapping && seedEndInMapping = [SeedRange (seedStart - srcStart + dstStart) seedRange]
    -- seed range starts outside of mapping but ends inside it
    | not seedStartInMapping && seedEndInMapping =
        translateRange ms (SeedRange seedStart (srcStart - seedStart))
            <> [SeedRange dstStart (seedEnd - srcStart)]
    -- seed range starts inside of mapping but ends outside it
    | seedStartInMapping && not seedEndInMapping =
        [SeedRange (seedStart - srcStart + dstStart) (srcEnd - seedStart)]
            <> translateRange ms (SeedRange srcEnd (seedEnd - srcEnd))
    -- seed range starts and ends outside mapping, but middle chunk is inside it
    | seedWhollyContainsMapping =
        translateRange ms (SeedRange seedStart (srcStart - seedStart))
            <> [SeedRange dstStart range]
            <> translateRange ms (SeedRange srcEnd (seedEnd - srcEnd))
    -- fallback - there is absolutely no overlap between mapping and range. Try next mapping
    | otherwise = translateRange ms s
  where
    srcEnd = srcStart + range
    seedEnd = seedStart + seedRange
    seedStartInMapping = seedStart >= srcStart && seedStart < srcEnd
    seedEndInMapping = seedEnd > srcStart && seedEnd <= srcEnd
    seedWhollyContainsMapping = seedStart < srcStart && seedEnd > srcEnd
translateRange [] s = [s]

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
parseSeedRanges seedsRow = parseSeedRange <$> chunks 2 (words seedsRow)

parseSeedRange :: [String] -> SeedRange
parseSeedRange [x, y] = SeedRange (read x) (read y)
parseSeedRange _ = error "malformed seed range"

parseMappingRow :: String -> Mapping
parseMappingRow mappingRow =
    case words mappingRow of
        [x, y, z] -> Mapping (read y) (read x) (read z)
        _ -> error $ "malformed mapping row " <> mappingRow
