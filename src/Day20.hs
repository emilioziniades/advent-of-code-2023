{-# LANGUAGE TupleSections #-}

module Day20 (measurePulses, broadcastLowRx, broadcastLowRxDirect, countUntilRxLowPulse, countUntilLowRxDirect) where

import Data.Bifunctor
import Data.Char (isAlpha, isPunctuation, isSpace)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tuple (swap)
import Util.Maths (lcmList)

type ModuleLabel = String

type Modules = Map.Map ModuleLabel Module

data ModuleState = Broadcaster | FlipFlop Bool | Conjunction (Map.Map ModuleLabel PulseType)
    deriving (Eq, Show)

data Module = Module {getState :: ModuleState, getDestinationLabels :: [ModuleLabel]}
    deriving (Show)

data PulseType = High | Low
    deriving (Eq, Show)

data Mod = Broadcaster_ | FlipFlop_ String | Conjunction_ String | RXTarget
    deriving (Eq, Ord, Show)

-- Part 1

measurePulses :: FilePath -> IO Int
measurePulses filename = do
    file <- readFile filename
    let modules = prepopulateConjunctionModules $ parseInput file
    print modules
    pure $ uncurry (*) $ broadcastNPulses modules 1000

broadcastNPulses :: Modules -> Int -> (Int, Int)
broadcastNPulses modules n
    | n == 1 = signalsCount
    | otherwise = signalsCount `bisum` broadcastNPulses newModules (n - 1)
  where
    (signalsCount, newModules) = processPulses modules [("button", "broadcaster", Low)]
    bisum x = bimap (+ fst x) (+ snd x)

processPulses :: Modules -> [(ModuleLabel, ModuleLabel, PulseType)] -> ((Int, Int), Modules)
processPulses modules [] = ((0, 0), modules)
processPulses modules ((srcLabel, dstLabel, pulse) : ps) = first increment (processPulses newModules newPulses)
  where
    increment = case pulse of
        High -> second (+ 1)
        Low -> first (+ 1)
    dstModule = Map.lookup dstLabel modules
    (newModules, newPulses) = case dstModule of
        Just (Module state dstLabels) ->
            let newState = updateModule srcLabel pulse state
             in ( Map.insert dstLabel (Module newState dstLabels) modules
                , ps <> processPulse newModules (srcLabel, dstLabel, pulse)
                )
        Nothing -> (modules, ps)

-- Part 2

broadcastLowRx :: FilePath -> IO Int
broadcastLowRx filename = do
    file <- readFile filename
    let modules = (prepopulateConjunctionModules . parseInput) file
    pure $ getLowRxPulse modules 1

broadcastLowRxDirect :: String -> Int
broadcastLowRxDirect file = getLowRxPulse modules 1
  where
    modules = (prepopulateConjunctionModules . parseInput) file

getLowRxPulse :: Modules -> Int -> Int
getLowRxPulse modules n
    | hasLowRxPulse = n
    | otherwise = getLowRxPulse newModules (n + 1)
  where
    (hasLowRxPulse, newModules) = findLowPulse "rx" modules [("button", "broadcaster", Low)]

findLowPulse :: String -> Modules -> [(ModuleLabel, ModuleLabel, PulseType)] -> (Bool, Modules)
findLowPulse _ modules [] = (False, modules)
findLowPulse targetModule modules ((srcLabel, dstLabel, pulse) : ps)
    | dstLabel == targetModule && pulse == Low = (True, modules)
    | otherwise = findLowPulse targetModule newModules newPulses
  where
    dstModule = Map.lookup dstLabel modules
    (newModules, newPulses) = case dstModule of
        Just (Module state dstLabels) ->
            let newState = updateModule srcLabel pulse state
             in ( Map.insert dstLabel (Module newState dstLabels) modules
                , ps <> processPulse newModules (srcLabel, dstLabel, pulse)
                )
        Nothing -> (modules, ps)

-- START OF PART 2 REWRITE

-- input parsing

countUntilLowRxDirect :: String -> Int
countUntilLowRxDirect input = countUntilRxLowPulse dst srcs reversedModMap
  where
    modMap = parseInputPart2 input
    -- modMap = traceShowId $ parseInputPart2 input
    reversedModMap = reverseMap modMap
    -- reversedModMap = traceShowId $ reverseMap modMap
    dst = RXTarget
    srcs = Map.lookup dst reversedModMap

reverseMap :: (Ord v, Ord k) => Map.Map k [v] -> Map.Map v [k]
reverseMap m = foldr updateListValues Map.empty swapped
  where
    swapped = swap <$> Map.toList m

updateListValues :: (Ord v, Ord k) => ([k], v) -> Map.Map k [v] -> Map.Map k [v]
updateListValues (ks, v) prev = foldr updateListValue prev kvs
  where
    kvs = fmap (,v) ks

updateListValue :: (Ord k) => (k, v) -> Map.Map k [v] -> Map.Map k [v]
updateListValue (key, value) = Map.insertWith (++) key [value]

-- Part 2

{-
TODO: alright, I have found a fatal issue with my logic. conjunctions don't always receive pulses from
all inputs on each button press.

If there is a chain of flipflips, then one flipflop might send high,
and the rest of the flipflops don't send anything. So it means that this "numerical" approach to the
solution might be flawed. This is the example that was plaguing me in the shower yesterday:
broadcaster -> a, c

%a -> b
%b -> d
%c -> d
&d -> rx

After three button presses, d remembers high from b (from the second press), and gets high from c. So
it activates.

Therefore, the LCM approach does not work for conjunctions.

I see two ways forward:

1. Use the same approach but figure out how to do conjunctions. It's definitely not using `lcm`.
2. Scrap the whole approach and start again *cries*.

 -}

-- Map.Map Mod [Mod]  is a map Destination -> Sources
countUntilRxLowPulse :: Mod -> Maybe [Mod] -> Map.Map Mod [Mod] -> Int
-- countUntilRxLowPulse dst srcs _ | traceShow (dst, srcs) False = undefined
countUntilRxLowPulse Broadcaster_ Nothing _ = 1
countUntilRxLowPulse Broadcaster_ _ _ = error "broadcaster should never have a source"
countUntilRxLowPulse RXTarget (Just [src]) modules = countUntilRxLowPulse src (Map.lookup src modules) modules
countUntilRxLowPulse RXTarget _ _ = error "rx should have only one source"
countUntilRxLowPulse (FlipFlop_ _) (Just [src]) modules = 2 * countUntilRxLowPulse src (Map.lookup src modules) modules
countUntilRxLowPulse (FlipFlop_ _) _ _ = error "flipflops can only have one source"
-- TODO: patterns below need reworking
countUntilRxLowPulse (Conjunction_ _) (Just srcs) modules = intDiv nextCount 2
  where
    countNext src = countUntilRxLowPulse src (Map.lookup src modules) modules
    nextCounts = fmap countNext srcs
    nextCount = lcmList nextCounts
countUntilRxLowPulse (Conjunction_ _) Nothing _ = error "conjunctions will always have sources"

-- integer division which throws an error if there is loss in division
intDiv :: Int -> Int -> Int
intDiv x y
    | isInt z = floor z
    | otherwise = error $ "cannot be losslessly converted to int: " <> show z
  where
    z :: Float
    z = fromIntegral x / fromIntegral y
    isInt :: Float -> Bool
    isInt n = fromInteger (round n) == n && not (isInfinite n)

parseInputPart2 :: String -> Map.Map Mod [Mod]
parseInputPart2 file = Map.fromList modMap
  where
    lists = fmap (parseRowPart2 . words) (lines file)
    (srcDstList, modTypeList) = unzip lists
    modTypeMap = Map.fromList modTypeList
    getType label =
        if label == "rx"
            then RXTarget
            else fromJust $ Map.lookup label modTypeMap
    mapTuple f g (a1, a2) = (f a1, g a2)
    modMap = fmap (mapTuple getType (fmap getType)) srcDstList

parseRowPart2 :: [String] -> ((String, [String]), (String, Mod))
parseRowPart2 (srcRaw : "->" : dstsRaw) = ((src, dsts), (src, getModule srcRaw))
  where
    removePunctuation = filter (not . isPunctuation)
    src = removePunctuation srcRaw
    dsts = fmap removePunctuation dstsRaw
    getModule :: String -> Mod
    getModule "broadcaster" = Broadcaster_
    getModule ('%' : label) = FlipFlop_ label
    getModule ('&' : label) = Conjunction_ label
    getModule _ = error "malformed input"
parseRowPart2 _ = error "malformed input"

-- END OF PART 2 REWRITE

-- Common to Part 1 and 2

processPulse :: Modules -> (ModuleLabel, ModuleLabel, PulseType) -> [(ModuleLabel, ModuleLabel, PulseType)]
processPulse modules (_, dstLabel, pulse) = makeSignals dstLabel currentModule pulse
  where
    currentModule = fromJust $ Map.lookup dstLabel modules

makeSignals :: ModuleLabel -> Module -> PulseType -> [(ModuleLabel, ModuleLabel, PulseType)]
makeSignals label (Module Broadcaster nextLabels) _ = fmap (label,,Low) nextLabels
makeSignals label (Module (FlipFlop on) nextLabels) Low = fmap (label,,if on then High else Low) nextLabels
makeSignals _ (Module (FlipFlop _) _) High = []
makeSignals label (Module (Conjunction prevSignals) nextLabels) _
    | null (Map.elems prevSignals) = fmap (label,,High) nextLabels
    | all (== High) (Map.elems prevSignals) = fmap (label,,Low) nextLabels
    | otherwise = fmap (label,,High) nextLabels

updateModule :: ModuleLabel -> PulseType -> ModuleState -> ModuleState
updateModule _ Low (FlipFlop on) = FlipFlop (not on)
updateModule _ High (FlipFlop on) = FlipFlop on
updateModule _ _ Broadcaster = Broadcaster
updateModule srcLabel pulse (Conjunction pulses) = Conjunction (Map.insert srcLabel pulse pulses)

prepopulateConjunctionModules :: Modules -> Modules
prepopulateConjunctionModules modules = Map.union (Map.fromList updatedConjunctionModules) modules
  where
    conjunctionModules = filter (isConjunction . snd) $ Map.toList modules
    updatedConjunctionModules = fmap (updateConjunctionModules modules) conjunctionModules
    isConjunction (Module (Conjunction _) _) = True
    isConjunction _ = False

updateConjunctionModules :: Modules -> (ModuleLabel, Module) -> (ModuleLabel, Module)
updateConjunctionModules modules (l, m) = (l, newM)
  where
    sourceModules = Map.fromList $ (,Low) <$> getSourceModules modules l
    newM = updateConjunctionModuleSources m sourceModules

getSourceModules :: Modules -> ModuleLabel -> [ModuleLabel]
getSourceModules modules destinationLabel = fst <$> filter (elem destinationLabel . getDestinationLabels . snd) (Map.toList modules)

updateConjunctionModuleSources :: Module -> Map.Map ModuleLabel PulseType -> Module
updateConjunctionModuleSources (Module (Conjunction _) destinations) newSources = Module (Conjunction newSources) destinations
updateConjunctionModuleSources _ _ = error "this function only works on conjunction modules"

-- Input parsing

parseInput :: String -> Modules
parseInput file = Map.fromList $ fmap parseRow (lines file)

parseRow :: String -> (ModuleLabel, Module)
parseRow row = (label, Module m (tail ws))
  where
    ws = words $ filter (\c -> or $ [isSpace, isAlpha, (== '&'), (== '%')] <*> pure c) row
    (label, m) = parseModule (head ws)

parseModule :: String -> (ModuleLabel, ModuleState)
parseModule "broadcaster" = ("broadcaster", Broadcaster)
parseModule ('%' : label) = (label, FlipFlop False)
parseModule ('&' : label) = (label, Conjunction Map.empty)
parseModule x = error $ x <> ": malformed module"
