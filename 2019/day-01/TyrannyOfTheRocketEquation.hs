-- https://adventofcode.com/2019/day/1

-- module TyrannyOfTheRocketEquation where

import System.Environment

readLines :: String -> IO [String]
readLines = fmap lines . readFile

parseToInt :: String -> Integer
parseToInt = read

fuelRequirement :: Integer -> Integer
fuelRequirement = subtract 2 . flip div 3

main :: IO ()
main = do
  input <- fmap (map parseToInt) (readLines "input")
  print $ sum (map fuelRequirement input)
