module Day06 (recordBreakingProduct, recordBreakingRace) where

import Control.Applicative
import Data.Char
import Util.Maths

type Time = Double
type Distance = Double

-- Part 1

recordBreakingProduct :: FilePath -> IO Int
recordBreakingProduct filename = do
    file <- readFile filename
    pure $ product $ countRecordBreaks <$> parseRaces file

-- Part 2

recordBreakingRace :: FilePath -> IO Int
recordBreakingRace filename = do
    file <- readFile filename
    pure $ countRecordBreaks $ parseSingleRace file

-- Common to Part 1 and Part 2

{-
    d = distance, th = time_holding, tT = total_time
    d = th * (tT - th)
    d = - th ^ 2 + tT * th
    0 = - th ^ 2 + tT * th - d
    i.e. quadratic roots where a = -1, b = tT, c = -d
-}
countRecordBreaks :: (Time, Distance) -> Int
countRecordBreaks (time, distance) =
    let (x1, x2) = quadraticRoots (-1) time (-distance)
     in ceiling x2 - floor x1 - 1

-- Input parsing

parseRaces :: String -> [(Time, Distance)]
parseRaces file = zip (fmap read timeLine) (fmap read distanceLine)
  where
    fileLines = lines $ filter (not . liftA2 (||) isLetter isPunctuation) file
    timeLine = words $ head fileLines
    distanceLine = words $ fileLines !! 1

parseSingleRace :: String -> (Time, Distance)
parseSingleRace file = (read $ head l, read $ l !! 1)
  where
    l = filter isDigit <$> lines file
