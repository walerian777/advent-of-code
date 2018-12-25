-- https://adventofcode.com/2018/day/25

module FourDimensionalAdventure where

import Data.List.Split (splitOn)
import Data.List (sort, partition, foldl', union)

type Point = (Int, Int, Int, Int)
type Constellation = [Point]

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

toPoint :: String -> Point
toPoint str = (w, x, y, z)
  where (w:x:y:z:_) = map read (splitOn "," str)

manhattanDistance :: Point -> Point -> Int
manhattanDistance (p1, p2, p3, p4) (q1, q2, q3, q4)
  = abs (p1 - q1) + abs (p2 - q2) + abs (p3 - q3) + abs (p4 - q4)

constellation :: Point -> [Point] -> (Constellation, [Point])
constellation p ps =
    let (near, far)  = partition ((<= 3) . manhattanDistance p) ps
        (same, diff) = foldl' (\(n, f) p -> let (s, d) = constellation p f in (union n s, d)) ([], far) near
    in  (p : union same near, diff)

constellations :: [Point] -> [Constellation]
constellations = constellations' []

constellations' :: [Constellation] -> [Point] -> [Constellation]
constellations' acc [] = acc
constellations' acc points = constellations' (const:acc) others
  where
  (p:ps) = sort points
  (const, others) = constellation p ps

main :: IO ()
main = do
  points <- fmap (map toPoint) (readLines "input")
  print $ length (constellations points)
