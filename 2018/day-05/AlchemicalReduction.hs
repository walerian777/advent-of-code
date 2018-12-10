-- https://adventofcode.com/2018/day/5

module AlchemicalReduction where

import Data.Char (toLower)

readLines :: String -> IO [String]
readLines = fmap lines . readFile

reducedPolymerLenght :: String -> Int
reducedPolymerLenght = length . foldr reaction []

reaction :: Char -> String -> String
reaction x acc
  | (not . null) acc && toLower x == toLower y && x /= y = tail acc
  | otherwise = (x:acc)
  where y = head acc

shortestPolymerLength :: String -> Int
shortestPolymerLength = minimum . map reducedPolymerLenght . improvedPolymers

improvedPolymers :: String -> [String]
improvedPolymers polymer = [removeOccurences letter polymer | letter <- alphabet]
  where alphabet = ['a'..'z']

removeOccurences :: Char -> String -> String
removeOccurences _ [] = []
removeOccurences y (x:xs)
  | y == toLower x = removeOccurences y xs
  | otherwise = x : removeOccurences y xs

main :: IO ()
main = do
  polymer <- fmap head (readLines "input")
  print $ reducedPolymerLenght polymer
  print $ shortestPolymerLength polymer
