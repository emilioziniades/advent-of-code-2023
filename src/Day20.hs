{-# LANGUAGE TupleSections #-}

module Day20 (measurePulses, broadcastLowRx) where

import Data.Bifunctor
import Data.Char (isAlpha, isSpace)
import Data.Hash
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

type ModuleLabel = String

type Modules = Map.Map ModuleLabel Module

data ModuleState = Broadcaster | FlipFlop Bool | Conjunction (Map.Map ModuleLabel PulseType)
    deriving (Eq, Show)

instance Hashable ModuleState where
    hash Broadcaster = hashInt 0
    hash (FlipFlop bool) = combine (hashInt 1) (hash bool)
    hash (Conjunction srcs) = hash (Map.toAscList srcs)

data Module = Module {getState :: ModuleState, getDestinationLabels :: [ModuleLabel]}
    deriving (Show)

instance Hashable Module where
    hash (Module state labels) = combine (hash state) (hash labels)

data PulseType = High | Low
    deriving (Eq, Show)

instance Hashable PulseType where
    hash Low = hashInt 0
    hash High = hashInt 1

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
    let modules = (prepopulateConjunctionModules . parseInput) file
    pure $ getLowRxPulse Map.empty modules 1

getLowRxPulse :: Map.Map Hash Int -> Modules -> Int -> Int
getLowRxPulse _ modules n | traceShow (n, fingerprint modules) False = undefined
getLowRxPulse cache modules n
    | Map.member moduleHash cache = error "hit cycle!"
    | hasLowRxPulse = n
    | otherwise = getLowRxPulse (Map.insert moduleHash n cache) newModules (n + 1)
  where
    (hasLowRxPulse, newModules) = findLowPulse "qt" modules [("button", "broadcaster", Low)]
    moduleHash = fingerprint modules

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

fingerprint :: Modules -> Hash
fingerprint ms = hash (Map.toAscList ms)

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
