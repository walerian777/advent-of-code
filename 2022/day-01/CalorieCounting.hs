-- https://adventofcode.com/2022/day/1

module CalorieCounting where

readLines :: String -> IO [String]
readLines = fmap lines . readFile

type Calorie = Integer

toCalories :: [String] -> [Calorie]
toCalories xs = toCalories' xs [0]

toCalories' :: [String] -> [Calorie] -> [Calorie]
toCalories' [] calories = calories
toCalories' ("":xs) calories = toCalories' xs (0:calories)
toCalories' (x:xs) (c:calories) = toCalories' xs (c + (read x):calories)

main :: IO ()
main = do
  calories <- fmap toCalories (readLines "input")
  print $ maximum calories
