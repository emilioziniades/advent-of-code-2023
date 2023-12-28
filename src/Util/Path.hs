module Util.Path (aStar) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.PSQueue as PSQ
import Prelude hiding (Left, Right)

type Frontier a = PSQ.PSQ a Int
type CameFrom a = Map.Map a (Maybe a)
type CostSoFar a = Map.Map a Int

type NeighbourFn a = a -> [a]
type GoalCheckFn a = a -> Bool
type CostFn a = a -> Int
type HeuristicFn a = a -> Int

aStar :: (Ord a) => NeighbourFn a -> GoalCheckFn a -> CostFn a -> HeuristicFn a -> Frontier a -> CameFrom a -> CostSoFar a -> Int
aStar getNeighbours isAtGoal getCost getHeuristic queue cameFrom costSoFar
    | PSQ.null queue = error "queue exhausted before target reached"
    | isAtGoal current = fromJust $ Map.lookup current costSoFar
    | otherwise = aStar getNeighbours isAtGoal getCost getHeuristic newQueue newCameFrom newCostSoFar
  where
    (currentBinding, restQueue) = fromJust (PSQ.minView queue)
    current = PSQ.key currentBinding
    nexts = getNeighbours current
    (newQueue, newCameFrom, newCostSoFar) = foldr (updateState current getCost getHeuristic) (restQueue, cameFrom, costSoFar) nexts

updateState :: (Ord a) => a -> CostFn a -> HeuristicFn a -> a -> (Frontier a, CameFrom a, CostSoFar a) -> (Frontier a, CameFrom a, CostSoFar a)
updateState current getCost getHeuristic next (frontier, cameFrom, costSoFar)
    | Map.notMember next costSoFar || newCost < existingNewCost = (newFrontier, newCameFrom, newCostSoFar)
    | otherwise = (frontier, cameFrom, costSoFar)
  where
    currentCost = fromJust (Map.lookup current costSoFar)
    nextCost = getCost next
    newCost = currentCost + nextCost
    priority = newCost + getHeuristic next
    existingNewCost = fromJust (Map.lookup next costSoFar)
    newFrontier = PSQ.insert next priority frontier
    newCameFrom = Map.insert next (Just current) cameFrom
    newCostSoFar = Map.insert next newCost costSoFar
