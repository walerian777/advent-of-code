-- https://adventofcode.com/2022/day/3

module RucksackReorganization where

import Data.Map (Map, fromList, (!))
import Data.List.Split (chunksOf)

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

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = filter (`elem` xs) ys

rucksackIntersection :: Rucksack -> [Item]
rucksackIntersection (xs, ys) = intersection xs ys

groupIntersection :: [[Item]] -> [Item]
groupIntersection (x:y:z:_) = intersection (intersection x y) (intersection y z)

main :: IO ()
main = do
  input <- readLines "input"
  print $ sum $ map (getPriority . head . rucksackIntersection . toRucksack) input
  print $ sum $ map (getPriority . head . groupIntersection) (chunksOf 3 input)
