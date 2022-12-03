-- https://adventofcode.com/2022/day/2

module RockPaperScissors where

type Points = Integer

data Shape = Rock | Paper | Scissors
  deriving (Show, Eq)

instance Ord Shape where
  compare Rock Scissors = GT
  compare Rock Paper = LT
  compare Rock Rock = EQ
  compare Paper Rock = GT
  compare Paper Scissors = LT
  compare Paper Paper = EQ
  compare Scissors Paper = GT
  compare Scissors Rock = LT
  compare Scissors Scissors = EQ

result :: Shape -> Shape -> Points
result x y
  | x < y = 6
  | x == y = 3
  | otherwise = 0

toPoints :: Shape -> Points
toPoints Rock = 1
toPoints Paper = 2
toPoints Scissors = 3

toShape :: String -> Shape
toShape x
  | x == "A" || x == "X" = Rock
  | x == "B" || x == "Y" = Paper
  | x == "C" || x == "Z" = Scissors
  | otherwise = error "Oops"

toExpectedShape :: String -> Shape -> Shape
toExpectedShape x Rock
  | x == "X" = Scissors
  | x == "Y" = Rock
  | x == "Z" = Paper
  | otherwise = error "Oops"

toExpectedShape x Paper
  | x == "X" = Rock
  | x == "Y" = Paper
  | x == "Z" = Scissors
  | otherwise = error "Oops"

toExpectedShape x Scissors
  | x == "X" = Paper
  | x == "Y" = Scissors
  | x == "Z" = Rock
  | otherwise = error "Oops"

readLines :: String -> IO [String]
readLines = fmap lines . readFile

toTotalScore :: String -> Points
toTotalScore s = (result x y) + (toPoints y)
  where (x:y:_) = map toShape $ words s

toTotalScore' :: String -> Points
toTotalScore' s = (result x y) + (toPoints y)
  where
    (s1:s2:_) = words s
    x = toShape s1
    y = toExpectedShape s2 x

main :: IO ()
main = do
  input <- readLines "input"
  print $ sum $ map toTotalScore input
  print $ sum $ map toTotalScore' input
