module Day15 (sumHashes, configureBoxes) where

import Data.Char (digitToInt, isSpace, ord)
import qualified Data.Map as Map
import Util.Lists (enumerate, splitOn)

type BoxId = Int
type LensId = Int
type FocalLength = Int
type Label = String

type Boxes = Map.Map BoxId [Lens]

data Lens = Lens {getLabel :: Label, getLength :: FocalLength}
    deriving (Show)

data Action = Remove BoxId Label | Insert BoxId Lens
    deriving (Show)

-- Part 1

sumHashes :: FilePath -> IO Int
sumHashes filename = do
    file <- readFile filename
    pure $ sum $ hash <$> parseInput file

-- Part 2

configureBoxes :: FilePath -> IO Int
configureBoxes filename = do
    file <- readFile filename
    let actions = parseAction <$> parseInput file
    pure $ focusingPower $ foldl updateBoxes Map.empty actions

parseAction :: String -> Action
parseAction action = case reverse action of
    x : '=' : label -> Insert (hash (reverse label)) (Lens (reverse label) (digitToInt x))
    '-' : label -> Remove (hash (reverse label)) (reverse label)
    y -> error (y <> ": malformed action")

updateBoxes :: Boxes -> Action -> Boxes
updateBoxes boxes (Remove boxId label) = Map.adjust (filter ((/= label) . getLabel)) boxId boxes
updateBoxes boxes (Insert boxId lens) = Map.insertWith insertLens boxId [lens] boxes

insertLens :: [Lens] -> [Lens] -> [Lens]
insertLens [lens] lenses =
    if any ((== getLabel lens) . getLabel) lenses
        then replaceLens lens lenses
        else lenses <> [lens]
insertLens _ _ = error "can only insert one lens at a time"

replaceLens :: Lens -> [Lens] -> [Lens]
replaceLens newLens (oldLens : lenses)
    | getLabel newLens == getLabel oldLens = newLens : lenses
    | otherwise = oldLens : replaceLens newLens lenses
replaceLens _ [] = []

lensFocusingPower :: (Int, Int, Int) -> Int
lensFocusingPower (iBox, iLens, focalLength) = (1 + iBox) * (1 + iLens) * focalLength

focusingPower :: Boxes -> Int
focusingPower boxes = sum $ lensFocusingPower <$> concatMap flattenBox (Map.toList boxes)

flattenBox :: (BoxId, [Lens]) -> [(BoxId, LensId, FocalLength)]
flattenBox (boxId, lenses) = fmap (flattenLens boxId) (enumerate lenses)

flattenLens :: BoxId -> (Lens, LensId) -> (BoxId, LensId, FocalLength)
flattenLens boxId (lens, lensId) = (boxId, lensId, getLength lens)

-- Common to Part 1 and 2

hash :: String -> Int
hash = foldl (\n x -> ((n + ord x) * 17) `rem` 256) 0

-- Input parsing
parseInput :: String -> [String]
parseInput file = splitOn ',' (filter (not . isSpace) file)
