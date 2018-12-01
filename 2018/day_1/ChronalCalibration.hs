import System.Environment

trimPlus :: String -> String
trimPlus = filter (/= '+')

parseArgs :: [String] -> [Integer]
parseArgs = map (read . trimPlus)

main :: IO ()
main = do
  frequencies <- getArgs
  print $ sum (parseArgs frequencies)
