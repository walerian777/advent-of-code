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

totalPower :: FuelCell -> Int
totalPower (x, y) = sum (map powerLevel square)
  where
  square :: [FuelCell]
  square = [(k, l) | k <- [x..x + 2], l <- [y..y + 2]]

largestTotalPower :: FuelCell
largestTotalPower = maximumBy (comparing totalPower) grid
  where
  grid :: [FuelCell]
  grid = [(x, y) | x <- [1..298], y <- [1..298]]

main :: IO ()
main = do
  print $ largestTotalPower
