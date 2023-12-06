module Day06 (recordBreakingProduct) where

import Control.Applicative
import Data.Char

type Time = Int
type Distance = Int

recordBreakingProduct :: FilePath -> IO Int
recordBreakingProduct filename = do
    file <- readFile filename
    pure $ product $ countRecordBreaks <$> parseInput file

parseInput :: String -> [(Time, Distance)]
parseInput file = zip (fmap read timeLine) (fmap read distanceLine)
  where
    fileLines = lines $ filter (not . liftA2 (||) isLetter isPunctuation) file
    timeLine = words $ head fileLines
    distanceLine = words $ fileLines !! 1

countRecordBreaks :: (Time, Distance) -> Int
countRecordBreaks (time, distance) = length $ filter (> distance) $ distanceTravelled time [0 .. time]

distanceTravelled :: Int -> [Int] -> [Int]
distanceTravelled totalTime holdTimes = do
    holdTime <- holdTimes
    pure $ (totalTime - holdTime) * holdTime

-- distanceTravelled :: Int -> Int -> Int
-- distanceTravelled totalTime holdTime = (totalTime - holdTime) * holdTime
