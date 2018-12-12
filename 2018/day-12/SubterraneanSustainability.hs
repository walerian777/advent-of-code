-- https://adventofcode.com/2018/day/12

module SubterraneanSustainability where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (foldl')

type Pots = IntSet

initialState :: String
initialState = "..##.#######...##.###...#..#.#.#..#.##.#.##....####..........#..#.######..####.#.#..###.##..##..#..#"

futurePot :: String -> Bool
futurePot pattern
  | pattern == "..#.#" = True
  | pattern == ".#..." = True
  | pattern == "#####" = True
  | pattern == "#.###" = True
  | pattern == ".##.." = True
  | pattern == "#...#" = True
  | pattern == "####." = True
  | pattern == "..###" = True
  | pattern == "##..#" = True
  | pattern == "###.#" = True
  | pattern == "#.##." = True
  | pattern == ".##.#" = True
  | pattern == "##.##" = True
  | pattern == "...#." = True
  | otherwise = False

toPots :: String -> Pots
toPots = IntSet.fromList . map fst . filter ((==) '#' . snd) . zip [0..]

spread :: Pots -> Pots
spread pots = foldl' (flip ($)) IntSet.empty (map (spreadingFuction pots) [min..max])
  where
  min :: Int
  min = IntSet.findMin pots - 2
  max :: Int
  max = IntSet.findMax pots + 2

spreadingFuction :: Pots -> Int -> (Pots -> Pots)
spreadingFuction pots x
  | futurePot (toString pots x) = IntSet.insert x
  | otherwise = id

toString :: Pots -> Int -> String
toString pots x = map fn (map (flip IntSet.member pots) [(x - 2)..(x + 2)])
  where
  fn :: Bool -> Char
  fn True = '#'
  fn False = '.'

sumPots :: Pots -> Int
sumPots = sum . IntSet.toList

spreadOverGenerations :: Pots -> Int -> Pots
spreadOverGenerations = (!!) . iterate spread

sumOverGenerations :: Pots -> Int -> Int
sumOverGenerations = (sumPots .) . spreadOverGenerations

repeatableSpread :: Pots -> Int -> Int
repeatableSpread pots generation = sumFor generation - sumFor (generation - 1)
  where
  sumFor :: Int -> Int
  sumFor = sumOverGenerations pots

sumOverRepeatableGenerations pots generation = sum + pattern * (ultimateGeneration - generation)
  where
  ultimateGeneration :: Int
  ultimateGeneration = 50000000000
  pattern :: Int
  pattern = repeatableSpread pots generation
  sum :: Int
  sum = sumOverGenerations pots generation

main :: IO ()
main = do
  let pots = toPots initialState
  print $ sumOverGenerations pots 20
  print $ sumOverRepeatableGenerations pots 100
