module Day01 (sumCalibrationValues, sumCalibrationValuesLiteral) where

import Data.Char
import Data.List

sumCalibrationValues :: String -> IO Int
sumCalibrationValues filename = do
    file <- readFile filename
    return $ sum $ map calibrationValue $ lines file

calibrationValue :: String -> Int
calibrationValue line =
    let
        numbers = filter isDigit line
        f = digitToInt $ head numbers
        l = digitToInt $ last numbers
     in
        f * 10 + l

sumCalibrationValuesLiteral :: String -> IO Int
sumCalibrationValuesLiteral filename = do
    file <- readFile filename
    return $ sum $ map calibrationValueLiteral $ lines file

literalNumbers :: [(String, Int)]
literalNumbers =
    [ ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    ]

calibrationValueLiteral :: String -> Int
calibrationValueLiteral line =
    let
        f = firstDigitLiteral line
        l = lastDigitLiteral line
     in
        f * 10 + l

firstDigitLiteral :: String -> Int
firstDigitLiteral line@(x : xs)
    | isDigit x = digitToInt x
    | otherwise =
        case find (\(l, _) -> l `isPrefixOf` line) literalNumbers of
            Just (_, n) -> n
            Nothing -> firstDigitLiteral xs
firstDigitLiteral [] = error "could not find first digit"

-- \| isDigit x = digitToInt x

lastDigitLiteral :: String -> Int
lastDigitLiteral line
    | isDigit (last line) = digitToInt (last line)
    | otherwise =
        case find (\(l, _) -> l `isSuffixOf` line) literalNumbers of
            Just (_, n) -> n
            Nothing -> lastDigitLiteral (init line)
