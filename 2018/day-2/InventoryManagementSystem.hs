-- https://adventofcode.com/2018/day/2

module InventoryManagementSystem where

import Data.List (group, sort)

readLines :: String -> IO [String]
readLines = fmap lines . readFile

calculateChecksum :: [String] -> Int
calculateChecksum input = countTwos * countThrees
  where
  countTwos = countOccurences 2 input
  countThrees = countOccurences 3 input

countOccurences :: Int -> [String] -> Int
countOccurences value list = length (filter (any (== value)) (map toOccurences list))

toOccurences :: String -> [Int]
toOccurences = map length . group . sort

main :: IO ()
main = do
  input <- readLines "input"
  print $ calculateChecksum input
