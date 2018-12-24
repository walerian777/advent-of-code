module ExperimentalEmergencyTeleportation where

import Data.List.Split (splitOn)
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.SBV as SBV

type Position = (Integer, Integer, Integer)
type SignalRadius = Integer

type Nanobot = (Position, SignalRadius)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

toInt :: String -> Integer
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

manhattanDistance :: Position -> Position -> Integer
manhattanDistance (p1, p2, p3) (q1, q2, q3)
  = abs (p1 - q1) + abs (p2 - q2) + abs (p3 - q3)

distance :: Nanobot -> Nanobot -> Integer
distance (pos1, _) (pos2, _) = manhattanDistance pos1 pos2

strongestNanobot :: [Nanobot] -> Nanobot
strongestNanobot = maximumBy (comparing snd)

nanobotsInRange :: Nanobot -> [Nanobot] -> [Nanobot]
nanobotsInRange target@(_, radius) bots = filter ((<= radius) . distance target) bots

countNanobotsInRange :: Nanobot -> [Nanobot] -> Int
countNanobotsInRange = (length .) . nanobotsInRange

z3Abs :: (SBV.OrdSymbolic a, Num a) => a -> a
z3Abs n = SBV.ite (n SBV..< 0) (negate n) n

z3ManhattanDistance :: (Num a, SBV.OrdSymbolic a)
  => (a, a, a) -> (a, a, a) -> a
z3ManhattanDistance (p1, p2, p3) (q1, q2, q3) =
  z3Abs (p1 - q1) + z3Abs (p2 - q2) + z3Abs (p3 - q3)

z3InRange :: (SBV.SymWord a1, SBV.SymWord a2, Num a1, Num a2)
  => (SBV.SBV a2, SBV.SBV a2, SBV.SBV a2) -> ((a2, a2, a2), a2) -> SBV.SBV a1
z3InRange (p1, p2, p3) ((q1, q2, q3), radius) = SBV.oneIf . (SBV..<= SBV.literal radius) $ z3ManhattanDistance
  ((SBV.literal q1), (SBV.literal q2), (SBV.literal q3)) (p1, p2, p3)

z3Model :: [Nanobot] -> IO SBV.OptimizeResult
z3Model nanobots = do
  SBV.optimize SBV.Lexicographic $ do
    [x, y, z] <- SBV.sIntegers ["x", "y", "z"]
    SBV.maximize "Nanobots in range" (sum (map ((\n -> n :: SBV.SInteger) . z3InRange (x, y, z)) nanobots))
    SBV.minimize "Distance to 0,0,0" (z3ManhattanDistance (0, 0, 0) (x, y, z))

main :: IO ()
main = do
  nanobots <- fmap (map toNanobot) (readLines "input")
  let strongest = strongestNanobot nanobots
  print $ countNanobotsInRange strongest nanobots

  model <- z3Model nanobots
  print $ model
