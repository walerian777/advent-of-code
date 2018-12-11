-- https://adventofcode.com/2018/day/11

module ChronalCharge where

import Data.List (maximumBy)
import Data.Ord (comparing)

type FuelCell = (Int, Int)

serialNumber :: Int
serialNumber = 5719

hundredsDigit :: Int -> Int
hundredsDigit x = div (mod x 1000) 100

powerLevel :: FuelCell -> Int
powerLevel (x, y) = hundredsDigit ((rackId * y + serialNumber) * rackId) - 5
  where
  rackId :: Int
  rackId = x + 10

square :: FuelCell -> Int -> [FuelCell]
square (x, y) z = [(k, l) | k <- [x..x + z], l <- [y..y + z]]

totalPower :: FuelCell -> Int
totalPower cell = sum (map powerLevel (square cell 2))

largestTotalPower :: FuelCell
largestTotalPower = maximumBy (comparing totalPower) grid
  where
  grid :: [FuelCell]
  grid = [(x, y) | x <- [1..298], y <- [1..298]]

anyTotalPower :: (FuelCell, Int) -> Int
anyTotalPower (cell, z) = sum (map powerLevel (square cell z))

anyLargestTotalPower :: (FuelCell, Int)
anyLargestTotalPower = maximumBy (comparing anyTotalPower) grid
  where
  grid :: [(FuelCell, Int)]
  grid = [((x, y), z) | z <- [1..300], x <- [1..300 - z + 1], y <- [1..300 - z + 1]]

main :: IO ()
main = do
  print $ largestTotalPower
  print $ anyLargestTotalPower
