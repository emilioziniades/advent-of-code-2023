module Day05 (lowestLocationNumber) where

import Data.Char
import Parse (splitOn)

type Seed = Int

data Almanac = Almanac {seeds :: [Seed], mappings :: [[Mapping]]} deriving (Show)

-- data Category = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location deriving (Show)

data Mapping = Mapping
    { destinationRangeStart :: Int
    , sourceRangeStart :: Int
    , rangeLength :: Int
    }
    deriving (Show)

lowestLocationNumber :: FilePath -> IO Int
lowestLocationNumber filename = do
    file <- readFile filename
    let almanac = parseInput file
    print almanac
    pure 0

parseInput :: String -> Almanac
parseInput file = Almanac (read <$> words seedsRow) ((fmap . fmap) parseMappingRow mappingsRows)
  where
    rows = lines file
    seedsRow = dropWhile (not . isNumber) $ head rows
    mappingsRows = filter (not . null) $ splitOn "" $ filter (not . any isLetter) $ tail rows

parseMappingRow :: String -> Mapping
parseMappingRow mappingRow =
    case words mappingRow of
        [x, y, z] -> Mapping (read x) (read y) (read z)
        _ -> error $ "malformed mapping row " <> mappingRow
