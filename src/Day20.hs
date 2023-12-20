{-# LANGUAGE TupleSections #-}

module Day20 (measurePulses) where

import Data.Bifunctor
import Data.Char (isAlpha, isSpace)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

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
    let (lows, highs) = broadcastNPulses modules 1000 1
    print (lows, highs)
    pure (lows * highs)

processPulses :: Modules -> [(ModuleLabel, ModuleLabel, PulseType)] -> ((Int, Int), Modules)
-- processPulses _ ((srcLabel, dstLabel, pulse) : _) | trace (srcLabel <> " -" <> show pulse <> "-> " <> dstLabel) False = undefined
processPulses modules [] = ((0, 0), modules)
processPulses modules ((srcLabel, dstLabel, pulse) : ps) = first increment (processPulses newModules newPulses)
  where
    increment = case pulse of
        High -> second (+ 1)
        Low -> first (+ 1)
    dstModule = Map.lookup dstLabel modules
    (newModules, newPulses) = case dstModule of
        Just (Module state dstLabels) ->
            let newState = updateModule dstLabel pulse state
             in ( Map.insert dstLabel (Module newState dstLabels) modules
                , ps <> processPulse newModules (srcLabel, dstLabel, pulse)
                )
        Nothing -> (modules, ps)

processPulse :: Modules -> (ModuleLabel, ModuleLabel, PulseType) -> [(ModuleLabel, ModuleLabel, PulseType)]
processPulse modules (_, dstLabel, pulse) = makeSignals dstLabel currentModule pulse
  where
    currentModule = fromJust $ Map.lookup dstLabel modules

makeSignals :: ModuleLabel -> Module -> PulseType -> [(ModuleLabel, ModuleLabel, PulseType)]
makeSignals label (Module Broadcaster nextLabels) _ = fmap (label,,Low) nextLabels
makeSignals label (Module (FlipFlop on) nextLabels) Low = fmap (label,,if on then High else Low) nextLabels
makeSignals _ (Module (FlipFlop _) _) High = []
makeSignals label (Module (Conjunction prevSignals) nextLabels) _
    | all (== High) (Map.elems prevSignals) = fmap (label,,Low) nextLabels
    | otherwise = fmap (label,,High) nextLabels

broadcastNPulses :: Modules -> Int -> Int -> (Int, Int)
broadcastNPulses modules total n
    | n >= total = signalsCount
    | otherwise = signalsCount `bisum` broadcastNPulses newModules total (n + 1)
  where
    (signalsCount, newModules) = processPulses modules [("button", "broadcaster", Low)]

bisum :: (Int, Int) -> (Int, Int) -> (Int, Int)
bisum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

updateModule :: ModuleLabel -> PulseType -> ModuleState -> ModuleState
updateModule _ Low (FlipFlop on) = FlipFlop (not on)
updateModule _ High (FlipFlop on) = FlipFlop on
updateModule _ _ Broadcaster = Broadcaster
updateModule srcLabel pulse (Conjunction pulses) = Conjunction (Map.insert srcLabel pulse pulses)

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
