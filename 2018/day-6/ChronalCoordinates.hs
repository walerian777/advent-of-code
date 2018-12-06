-- https://adventofcode.com/2018/day/6

module ChronalCoordinates where

import Data.List.Split (splitOneOf)
import Data.List (group, maximumBy, sort, sortOn)
import Data.Ord (comparing)

type Point = [Int]

readLines :: String -> IO [String]
readLines = fmap lines . readFile

split :: String -> [String]
split = splitOneOf [' ', ',']

compact :: [String] -> [String]
compact = filter (not . null)

toPoint :: String -> Point
toPoint = map read . compact . split

manhattanDistance :: Point -> Point -> Int
manhattanDistance = (foldr fn 0 .) . zip
  where
  fn :: ((Int, Int) -> Int -> Int)
  fn = (+) . abs . uncurry (-)

generateGrid :: [Point] -> [Point]
generateGrid points = [[x, y] | x <- [0..maxX], y <- [0..maxY]]
  where
  maxX :: Int
  maxX = head (maximumBy (comparing head) points)
  maxY :: Int
  maxY = head (tail (maximumBy (comparing tail) points))

closestPoints :: [Point] -> [Point] -> Point -> [Point]
closestPoints coordinates acc point
  | y == z = acc
  | otherwise = (x:acc)
  where
  distances :: [(Point, Int)]
  distances = zip coordinates (map (manhattanDistance point) coordinates)
  [(x, y), (w, z)] = take 2 $ sortOn snd distances

largestArea :: [Point] -> Int
largestArea points = maximum (map length coordinates)
  where
  coordinates :: [[Point]]
  coordinates = group . sort $ foldl fn [] grid
  grid :: [Point]
  grid = generateGrid points
  fn :: ([Point] -> Point -> [Point])
  fn = closestPoints points

sumDistances :: [Point] -> Point -> Int
sumDistances = (sum .) . flip (map . manhattanDistance)

locationsBelowLimit :: Int -> [Point] -> Int
locationsBelowLimit limit points = length (filter (< limit) distances)
  where
  grid :: [Point]
  grid = generateGrid points
  distances :: [Int]
  distances = map (sumDistances points) grid

main :: IO ()
main = do
  points <- fmap (map toPoint) (readLines "input")
  print $ largestArea points
  print $ locationsBelowLimit 10000 points
