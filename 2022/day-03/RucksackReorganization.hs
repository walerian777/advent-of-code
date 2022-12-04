-- https://adventofcode.com/2022/day/3

module RucksackReorganization where

import Data.Map (Map, fromList, (!))

type Priority = Integer
type Item = Char
type Rucksack = ([Item], [Item])

prioritiesMapping :: Map Item Priority
prioritiesMapping = fromList $ zip (['a'..'z'] ++ ['A'..'Z']) [1..52]

getPriority :: Item -> Priority
getPriority = (!) prioritiesMapping

midIndex :: String -> Int
midIndex s = div (length s) 2

toRucksack :: String -> Rucksack
toRucksack s = splitAt (midIndex s) s

readLines :: String -> IO [String]
readLines = fmap lines . readFile

intersection :: Rucksack -> [Item]
intersection (xs, ys) = filter (`elem` xs) ys

main :: IO ()
main = do
  rucksacks <- fmap (map toRucksack) (readLines "input")
  print $ sum $ map (getPriority . head . intersection) rucksacks
