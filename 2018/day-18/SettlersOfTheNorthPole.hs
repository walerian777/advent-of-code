-- https://adventofcode.com/2018/day/18

module SettlersOfTheNorthPole where

import Data.List (find)
import Data.Maybe (maybeToList)
import Control.Monad (ap)

type Coordinate = (Int, Int)
type Acre = (Coordinate, AcreType)
type Area = [Acre]

data AcreType = Ground | Tree | Lumberyard
  deriving (Show, Eq)

toArea :: [String] -> Area
toArea input = zip grid (map toAcre (concat input))
  where
  grid = [(x, y) | x <- [0..49], y <- [0..49]] :: [Coordinate]

toAcre :: Char -> AcreType
toAcre '.' = Ground
toAcre '|' = Tree
toAcre '#' = Lumberyard

morph :: Acre -> Area -> Acre
morph acre@(coord, acreType) surrounding
  | acreType == Ground
    && scanArea surrounding Tree >= 3 = (coord, Tree)
  | acreType == Tree
    && scanArea surrounding Lumberyard >= 3 = (coord, Lumberyard)
  | acreType == Lumberyard
    && scanArea surrounding Tree >= 1
    && scanArea surrounding Lumberyard >= 1 = (coord, Lumberyard)
  | acreType == Lumberyard = (coord, Ground)
  | otherwise = acre

scanArea :: Area -> AcreType -> Int
scanArea area acreType = length (filter ((== acreType) . snd) area)

epoch :: Area -> Area
epoch area = map (ap morph surrounding) area
  where
  surrounding :: Acre -> [Acre]
  surrounding (coord, _)  = map findAcre (neighbours coord)
  neighbours :: Coordinate -> [Coordinate]
  neighbours (x, y) = [(k, l)
                      | k <- [x - 1 .. x + 1]
                      , l <- [y - 1 .. y + 1]
                      , (k, l) /= (x, y)
                      ]
  findAcre :: Coordinate -> Acre
  findAcre coord = case find ((== coord) . fst) area of
    Just acre -> acre
    Nothing -> (coord, Ground)

resourceValue :: Area -> Int
resourceValue area = trees * lumberyards
  where
  trees = scanArea area Tree :: Int
  lumberyards =  scanArea area Lumberyard :: Int

main :: IO ()
main = do
  input <- fmap lines (readFile "input")
  let area = toArea input
  print $ resourceValue (iterate epoch area !! 10)
