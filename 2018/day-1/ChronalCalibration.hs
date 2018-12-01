-- https://adventofcode.com/2018/day/1
import System.Environment

trimPlus :: String -> String
trimPlus = filter (/= '+')

parseArgs :: [String] -> [Integer]
parseArgs = map (read . trimPlus)

getIntArgs :: IO [Integer]
getIntArgs = fmap parseArgs $ getArgs

reachedTwice :: [Integer] -> Integer
reachedTwice xs = reachedTwice' [0] xs xs

reachedTwice' :: [Integer] -> [Integer] -> [Integer] -> Integer
reachedTwice' sums [] list = reachedTwice' sums list list
reachedTwice' (x:xs) (y:ys) list =
  if elem sum (x:xs)
    then sum
    else reachedTwice' (sum:x:xs) ys list
  where sum = x + y

main :: IO ()
main = do
  args <- getIntArgs
  print $ sum args
  print $ reachedTwice args
