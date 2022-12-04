-- https://adventofcode.com/2022/day/4

module CampCleanup where

import Data.List.Split (splitOneOf)

type Section = Integer
type Assignment = (Section, Section)

parseLine :: String -> (Assignment, Assignment)
parseLine s = ((fromX, toX), (fromY, toY))
  where (fromX:toX:fromY:toY:_) = map read (splitOneOf ",-" s)

readLines :: String -> IO [String]
readLines = fmap lines . readFile

size :: Assignment -> Integer
size (from, to) = to - from + 1

overlap :: Assignment -> Assignment -> Assignment
overlap (fromX, toX) (fromY, toY) = (fromBigger, toSmaller)
  where
    fromBigger = if fromX > fromY then fromX else fromY
    toSmaller = if toX < toY then toX else toY

fullyContains :: (Assignment, Assignment) -> Bool
fullyContains (x, y) = (size z) >= (size x) || (size z >= size y)
  where z = overlap x y

anyOverlaps :: (Assignment, Assignment) -> Bool
anyOverlaps (x, y) = (size (overlap x y)) > 0

main :: IO ()
main = do
  sections <- fmap (map parseLine) (readLines "input")
  print $ length $ filter fullyContains sections
  print $ length $ filter anyOverlaps sections
