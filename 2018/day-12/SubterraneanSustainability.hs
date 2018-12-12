-- https://adventofcode.com/2018/day/12

module SubterraneanSustainability where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (foldl')

type Pots = IntSet

initialState :: String
initialState = "..##.#######...##.###...#..#.#.#..#.##.#.##....####..........#..#.######..####.#.#..###.##..##..#..#"

spreadPattern :: String -> Char
spreadPattern "#..#." = '.'
spreadPattern "..#.." = '.'
spreadPattern "..#.#" = '#'
spreadPattern "##.#." = '.'
spreadPattern ".#..." = '#'
spreadPattern "#...." = '.'
spreadPattern "#####" = '#'
spreadPattern ".#.##" = '.'
spreadPattern "#.#.." = '.'
spreadPattern "#.###" = '#'
spreadPattern ".##.." = '#'
spreadPattern "##..." = '.'
spreadPattern "#...#" = '#'
spreadPattern "####." = '#'
spreadPattern "#.#.#" = '.'
spreadPattern "#..##" = '.'
spreadPattern ".####" = '.'
spreadPattern "...##" = '.'
spreadPattern "..###" = '#'
spreadPattern ".#..#" = '.'
spreadPattern "##..#" = '#'
spreadPattern ".#.#." = '.'
spreadPattern "..##." = '.'
spreadPattern "###.." = '.'
spreadPattern "###.#" = '#'
spreadPattern "#.##." = '#'
spreadPattern "....." = '.'
spreadPattern ".##.#" = '#'
spreadPattern "....#" = '.'
spreadPattern "##.##" = '#'
spreadPattern "...#." = '#'
spreadPattern ".###." = '.'

toPots :: String -> Pots
toPots = IntSet.fromList . map fst . filter ((==) '#' . snd) . zip [0..]

spread :: Pots -> Pots
spread pots = foldl' (flip ($)) IntSet.empty (map (spreadingFuction pots) [min..max])
  where
  min :: Int
  min = IntSet.findMin pots - 5
  max :: Int
  max = IntSet.findMax pots + 5

spreadingFuction :: Pots -> Int -> (Pots -> Pots)
spreadingFuction pots x
  | spreadPattern (toString pots x) == '#' = IntSet.insert x
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

main :: IO ()
main = do
  let pots = toPots initialState
  print $ sumPots (spreadOverGenerations pots 20)
