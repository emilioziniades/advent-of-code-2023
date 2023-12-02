module Day01 (sumCalibrationValues, calibrationValue, calibrationValueLiteral) where

import Data.Char
import Data.List

sumCalibrationValues :: (String -> Int) -> String -> IO Int
sumCalibrationValues f filename = do
    file <- readFile filename
    pure $ (sum . fmap f . lines) file

calibrationValue :: String -> Int
calibrationValue line =
    let
        numbers = filter isDigit line
        f = digitToInt $ head numbers
        l = digitToInt $ last numbers
     in
        f * 10 + l

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
        let startsWith (l, _) = l `isPrefixOf` line
         in case find startsWith literalNumbers of
                Just (_, n) -> n
                Nothing -> firstDigitLiteral xs
firstDigitLiteral [] = error "could not find first digit"

lastDigitLiteral :: String -> Int
lastDigitLiteral line
    | isDigit (last line) = digitToInt (last line)
    | otherwise =
        let endsWith (l, _) = l `isSuffixOf` line
         in case find endsWith literalNumbers of
                Just (_, n) -> n
                Nothing -> lastDigitLiteral (init line)

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
