module Day15 (sumHashes, configureBoxes) where

import Data.Bifunctor
import Data.Char (digitToInt, isSpace, ord)
import qualified Data.Map as Map
import Debug.Trace (traceId, traceShow)
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
    print actions
    let boxes = Map.empty
    pure $ focusingPower $ foldl updateBoxes boxes actions

parseAction :: String -> Action
parseAction action = traceShow action $ case reverse action of
    x : '=' : label -> Insert (hash (reverse label)) (Lens label (digitToInt x))
    '-' : label -> Remove (hash (reverse label)) label
    y -> error (y <> ": malformed action")

updateBoxes :: Boxes -> Action -> Boxes
updateBoxes _ action | traceShow action False = undefined
updateBoxes boxes _ | traceShow boxes False = undefined
updateBoxes boxes (Remove boxId label) = Map.adjust (filter ((/= label) . getLabel)) boxId boxes
updateBoxes boxes (Insert boxId (Lens label focalLength)) = Map.adjust (insertLens label focalLength) boxId boxes

insertLens :: Label -> FocalLength -> [Lens] -> [Lens]
insertLens label focalLength lenses =
    if any ((== label) . getLabel) lenses
        then error "haven't figured this out yet"
        else lenses <> [Lens label focalLength]

lensFocusingPower :: (Int, Int, Int) -> Int
lensFocusingPower (iBox, iLens, focalLength) = (1 + iBox) * (1 + iLens) * focalLength

focusingPower :: Boxes -> Int
focusingPower boxes | traceShow boxes False = undefined
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
