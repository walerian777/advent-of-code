-- https://adventofcode.com/2018/day/2

module InventoryManagementSystem where

import Data.List (group, sort, intersect, (\\))

readLines :: String -> IO [String]
readLines = fmap lines . readFile

calculateChecksum :: [String] -> Int
calculateChecksum list = countTwos * countThrees
  where
  countTwos = countOccurences 2 list
  countThrees = countOccurences 3 list

countOccurences :: Int -> [String] -> Int
countOccurences value list = length (filter (any (== value)) (map toOccurences list))

toOccurences :: String -> [Int]
toOccurences = map length . group . sort

commonLetters :: [String] -> String
commonLetters list = head [intersect xs ys | xs <- list, ys <- list, differByOneCharacter xs ys]

differByOneCharacter :: String -> String -> Bool
differByOneCharacter xs ys = length (xs \\ ys) == 1

main :: IO ()
main = do
  input <- readLines "input"
  print $ calculateChecksum input
  print $ commonLetters input
