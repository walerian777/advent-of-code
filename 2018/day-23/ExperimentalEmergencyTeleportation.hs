module ExperimentalEmergencyTeleportation where

import Data.List.Split (splitOn)
import Data.List (maximumBy)
import Data.Ord (comparing)

type Position = (Int, Int, Int)
type SignalRadius = Int

type Nanobot = (Position, SignalRadius)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

toInt :: String -> Int
toInt = read

toNanobot :: String -> Nanobot
toNanobot str = ((x, y, z), radius)
  where
  left :: String
  left = takeWhile (/= '>') (drop 5 str)
  right :: String
  right = drop 2 (dropWhile (/= 'r') str)
  (x:y:z:_) = map toInt (splitOn "," left)
  radius :: SignalRadius
  radius = toInt right

manhattanDistance :: Position -> Position -> Int
manhattanDistance (p1, p2, p3) (q1, q2, q3)
  = abs (p1 - q1) + abs (p2 - q2) + abs (p3 - q3)

distance :: Nanobot -> Nanobot -> Int
distance (pos1, _) (pos2, _) = manhattanDistance pos1 pos2

strongestNanobot :: [Nanobot] -> Nanobot
strongestNanobot = maximumBy (comparing snd)

nanobotsInRange :: Nanobot -> [Nanobot] -> [Nanobot]
nanobotsInRange target@(_, radius) bots = filter ((<= radius) . distance target) bots

main :: IO ()
main = do
  nanobots <- fmap (map toNanobot) (readLines "input")
  let strongest = strongestNanobot nanobots
  print $ length (nanobotsInRange strongest nanobots)
