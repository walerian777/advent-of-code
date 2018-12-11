-- https://adventofcode.com/2018/day/10

module TheStarsAlign where

import Data.List.Split(chunksOf, splitOneOf)
import Data.Semigroup(Max(Max), Min(Min))
import Data.Array(accumArray, elems)
import Data.Bool(bool)
import Data.Foldable(toList)
import Control.Monad(ap)

type Position = (Int, Int)
type Velocity = (Int, Int)
type Point = (Position, Velocity)
type Box = (Int, Int, Int, Int)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

toInt :: String -> Int
toInt = read

compact :: [String] -> [String]
compact = filter (not . null)

toPoint :: String -> Point
toPoint line = ((x, y), (dx, dy))
  where (x:y:dx:dy:_) = toPoint' line

toPoint' :: String -> [Int]
toPoint' = map toInt . compact . splitOneOf "velocitypsn=< ,>"

accelerate :: Point -> Point
accelerate ((x, y), (dx, dy)) = ((x + dx, y + dy), (dx, dy))

accelerateAll :: [Point] -> [[Point]]
accelerateAll = iterate (map accelerate)

boundingBox :: [Point] -> Box
boundingBox points = (minX, minY, maxX, maxY)
  where
  (Min minX, Min minY, Max maxX, Max maxY) = flip foldMap points fn
  fn :: Point -> (Min Int, Min Int, Max Int, Max Int)
  fn = \((x, y), _) -> (Min x, Min y, Max x, Max y)

visualise :: (Int, Int, Int, Int) -> [Point] -> String
visualise (minX, minY, maxX, maxY) =
  unlines . chunksOf (maxX - minX + 1) . map (bool '.' '#') . elems .
  accumArray (||) False ((minY, minX), (maxY, maxX)) . map fn . toList
  where
  fn :: Point -> (Position, Bool)
  fn ((px, py), _) = ((py, px), True)

compareBoxes :: Box -> Box -> Bool
compareBoxes box box' = (maxX - minX + 1) * (maxY - minY + 1) < (maxX' - minX' + 1) * (maxY' - minY' + 1)
  where
  (minX, minY, maxX, maxY) = box
  (minX', minY', maxX', maxY') = box'

watch :: [Point] -> [(Int, String)]
watch points = [
  (time, visualise box result)
  | (time, (result, box), (_, box')) <- zip3 [0..] `ap` tail $ (,) `ap` boundingBox <$> accelerateAll points, compareBoxes box box'
  ]

main :: IO ()
main = do
  points <- fmap (map toPoint) (readLines "input")
  let results = head (watch points)
  putStr $ snd results
  print $ fst results
