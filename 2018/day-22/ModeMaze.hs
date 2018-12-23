-- https://adventofcode.com/2018/day/22

{-# LANGUAGE MultiWayIf, TupleSections, DeriveGeneric #-}

module ModeMaze where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Ix (range)
import Data.Graph.AStar (aStar)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

type Coordinate = (Int, Int)
type Climbing = (Tool, Coordinate)

data RegionType = Rocky | Wet | Narrow
  deriving (Show, Eq)

data Tool = ClimbingGear | Torch | Neither
  deriving (Show, Eq, Ord, Generic)

instance Hashable Tool

caveDepth :: Int
caveDepth = 3066

caveMouth :: Coordinate
caveMouth = (0, 0)

target :: Coordinate
target = (13, 726)

erosionLevels :: Coordinate -> Map Coordinate Int
erosionLevels coord = levels
  where
  levels = fmap ((`mod` 20183) . (+ caveDepth)) geolocicalIndices
  geolocicalIndices = (`Map.fromSet` Set.fromList (range (caveMouth, coord))) $ \(x, y) ->
    if | (x, y) == caveMouth -> 0
       | (x, y) == target -> 0
       | y == 0 -> x * 16807
       | x == 0 -> y * 48271
       | otherwise -> (levels Map.! (pred x, y)) * (levels Map.! (x, pred y))

regionTypes :: Coordinate -> Map Coordinate RegionType
regionTypes = fmap (toRegionType . (`mod` 3)) . erosionLevels
  where
  toRegionType 0 = Rocky
  toRegionType 1 = Wet
  toRegionType 2 = Narrow

totalRisk :: Coordinate -> Int
totalRisk = sum . fmap toValue . regionTypes
  where
  toValue :: RegionType -> Int
  toValue Rocky = 0
  toValue Wet = 1
  toValue Narrow = 2

adequateTool :: RegionType -> Tool -> Bool
adequateTool Rocky ClimbingGear = True
adequateTool Rocky Torch = True
adequateTool Wet ClimbingGear = True
adequateTool Wet Neither = True
adequateTool Narrow Torch = True
adequateTool Narrow Neither = True
adequateTool _ _ = False

neighbours :: Coordinate -> [Coordinate]
neighbours (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

manhattanDistance :: Coordinate -> Coordinate -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

climbingDistance :: Climbing -> Climbing -> Int
climbingDistance (tool1, coord1) (tool2, coord2) = case tool1 == tool2 of
  True -> distance
  False -> distance + 7 
  where
  distance :: Int
  distance = manhattanDistance coord1 coord2

singleClimbingDistance :: Climbing -> Climbing -> Int
singleClimbingDistance (tool1, _) (tool2, _) = case tool1 == tool2 of
  True -> 1
  False -> 7

availableMoves :: Map Coordinate RegionType -> Climbing -> [Climbing]
availableMoves area (tool, coord) = filter (uncurry adequate) (xs ++ ys)
  where
  xs = map (, coord) . filter (/= tool) $ [Neither, ClimbingGear, Torch]
  ys = fmap (tool,) (neighbours coord)
  adequate :: Tool -> Coordinate -> Bool
  adequate x = maybe False (`adequateTool` x) . (`Map.lookup` area)

fastestWay :: Map Coordinate RegionType -> Maybe [Climbing]
fastestWay area = (start:) <$> aStar (HashSet.fromList . availableMoves area)
                                 singleClimbingDistance
                                 (climbingDistance finish)
                                 (== finish)
                                 start
  where
  start = (Torch, (0, 0)) :: Climbing
  finish = (Torch, target) :: Climbing

extendedTarget :: Coordinate
extendedTarget = (35, 800)

calculateTime :: [Climbing] -> Int
calculateTime = sum . map (uncurry singleClimbingDistance) . (zip <*> tail)

main :: IO ()
main = do
  print $ totalRisk target
  let area = regionTypes extendedTarget
  print $ fmap calculateTime (fastestWay area)
