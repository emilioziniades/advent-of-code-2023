{-# LANGUAGE TupleSections #-}

module Day20 (measurePulses, broadcastLowRx) where

import Data.Bifunctor
import Data.Char (isAlpha, isSpace)
import qualified Data.Map as Map
import Data.Maybe

type ModuleLabel = String

type Modules = Map.Map ModuleLabel Module

data ModuleState = Broadcaster | FlipFlop Bool | Conjunction (Map.Map ModuleLabel PulseType)
    deriving (Eq, Show)

data Module = Module {getState :: ModuleState, getDestinationLabels :: [ModuleLabel]}
    deriving (Show)

data PulseType = High | Low
    deriving (Eq, Show)

-- Part 1

measurePulses :: FilePath -> IO Int
measurePulses filename = do
    file <- readFile filename
    let modules = prepopulateConjunctionModules $ parseInput file
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
    let modules = prepopulateConjunctionModules $ parseInput file
    pure $ getLowRxPulse modules 1

getLowRxPulse :: Modules -> Int -> Int
getLowRxPulse modules n
    | hasLowRxPulse = n
    | otherwise = getLowRxPulse newModules (n + 1)
  where
    (hasLowRxPulse, newModules) = findLowRxPulse modules [("button", "broadcaster", Low)]

findLowRxPulse :: Modules -> [(ModuleLabel, ModuleLabel, PulseType)] -> (Bool, Modules)
findLowRxPulse modules [] = (False, modules)
findLowRxPulse modules ((srcLabel, dstLabel, pulse) : ps)
    | dstLabel == "rx" && pulse == Low = (True, modules)
    | otherwise = findLowRxPulse newModules newPulses
  where
    dstModule = Map.lookup dstLabel modules
    (newModules, newPulses) = case dstModule of
        Just (Module state dstLabels) ->
            let newState = updateModule srcLabel pulse state
             in ( Map.insert dstLabel (Module newState dstLabels) modules
                , ps <> processPulse newModules (srcLabel, dstLabel, pulse)
                )
        Nothing -> (modules, ps)

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
