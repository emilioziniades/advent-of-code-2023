module Day01 (sumCalibrationValues) where

import Data.Char

sumCalibrationValues :: String -> IO Int
sumCalibrationValues filename = do
    file <- readFile filename
    let calibrationValues = map (joinDigits . firstAndLastDigits) (lines file)
    return $ sum calibrationValues

firstAndLastDigits :: String -> (Int, Int)
firstAndLastDigits line =
    let
        numbers = filter isNumber line
        f = head numbers
        l = last numbers
     in
        (read [f], read [l])

joinDigits :: (Int, Int) -> Int
joinDigits (f, l) = (f * 10) + l
