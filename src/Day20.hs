{-# LANGUAGE TupleSections #-}

module Day20 (measurePulses) where

import Data.Char (isAlpha, isSpace)
import qualified Data.Map as Map
import Data.Maybe

type ModuleLabel = String

type Modules = Map.Map ModuleLabel Module

data ModuleState = Broadcaster | FlipFlop Bool | Conjunction (Map.Map ModuleLabel PulseType)
    deriving (Show)

data Module = Module ModuleState [ModuleLabel]
    deriving (Show)

data PulseType = High | Low
    deriving (Eq, Show)

measurePulses :: FilePath -> IO Int
measurePulses filename = do
    file <- readFile filename
    let modules = parseInput file
    print $ processPulses modules [("button", "broadcaster", Low)]
    print modules
    print file
    pure 0

processPulses :: Modules -> [(ModuleLabel, ModuleLabel, PulseType)] -> (Int, Int)
processPulses _ [] = (0, 0)
processPulses modules ((srcLabel, dstLabel, pulse) : ps) = count `bisum` processPulses newModules newPulses
  where
    count = case pulse of
        High -> (0, 1)
        Low -> (1, 0)
    (Module state dstLabels) = fromJust $ Map.lookup dstLabel modules
    newState = updateModule dstLabel pulse state
    newModules = Map.insert dstLabel (Module newState dstLabels) modules
    newPulses = ps <> processPulse newModules (srcLabel, dstLabel, pulse)

processPulse :: Modules -> (ModuleLabel, ModuleLabel, PulseType) -> [(ModuleLabel, ModuleLabel, PulseType)]
processPulse modules (_, dstLabel, pulse) = makeSignals dstLabel currentModule pulse
  where
    currentModule = fromJust $ Map.lookup dstLabel modules

makeSignals :: ModuleLabel -> Module -> PulseType -> [(ModuleLabel, ModuleLabel, PulseType)]
makeSignals label (Module Broadcaster nextLabels) _ = fmap (label,,Low) nextLabels
makeSignals label (Module (FlipFlop on) nextLabels) Low = fmap (label,,if on then Low else High) nextLabels
makeSignals _ (Module (FlipFlop _) _) High = []
makeSignals label (Module (Conjunction prevSignals) nextLabels) _
    | all (== High) (Map.elems prevSignals) = fmap (label,,Low) nextLabels
    | otherwise = fmap (label,,High) nextLabels

-- broadcastNPulses :: Modules -> Int -> Int -> (Int, Int)
-- broadcastNPulses modules total n
--     | total >= n = signalsCount
--     | otherwise = signalsCount `bimap` broadcastNPulses newModules total (n+1)

-- FlipFlop on -> let nextPulse = if pulse == Low then

bisum :: (Int, Int) -> (Int, Int) -> (Int, Int)
bisum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

updateModule :: ModuleLabel -> PulseType -> ModuleState -> ModuleState
updateModule _ Low (FlipFlop on) = FlipFlop (not on)
updateModule _ High (FlipFlop on) = FlipFlop on
updateModule _ _ Broadcaster = Broadcaster
updateModule srcLabel pulse (Conjunction pulses) = Conjunction (Map.insert srcLabel pulse pulses)

-- pulse -> target module -> state update and more pulses

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
