-- https://adventofcode.com/2018/day/5

module AlchemicalReduction where

import Data.Char (toLower)
import Debug.Trace

readLines :: String -> IO [String]
readLines = fmap lines . readFile

reducedPolymerLenght :: String -> Int
reducedPolymerLenght = length . foldr reaction []

reaction :: Char -> String -> String
reaction x acc
  | (not . null) acc && toLower x == toLower y && x /= y = tail acc
  | otherwise = (x:acc)
  where y = head acc

main :: IO ()
main = do
  polymer <- fmap head (readLines "input")
  print $ reducedPolymerLenght polymer
