-- https://adventofcode.com/2018/day/4

module ReposeRecord where

import Data.List.Split
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord

type GuardId = Int
type Minute = Int
type TimeLog = Map Minute Int

data Event = Begin GuardId | FallAsleep | WakeUp deriving (Show, Eq)
data Timestamp = Timestamp { year :: Int
                           , month :: Int
                           , day :: Int
                           , hour :: Int
                           , minute :: Minute
                           } deriving (Show, Eq, Ord)
data Record = Record { event :: Event
                     , timestamp :: Timestamp
                     } deriving (Show, Eq)

instance Ord Record where
  compare (Record _ (Timestamp _ month day hour minute)) (Record _ (Timestamp _ month' day' hour' minute'))
    | month == month' && day == day' && hour == hour' = compare minute minute'
    | month == month' && day == day' = compare hour hour'
    | month == month' = compare day day'
    | otherwise = compare month month'

splitOnDelimiters :: String -> [String]
splitOnDelimiters = splitOneOf [' ', '[', ']', '-', ':', '#']

compact :: [String] -> [String]
compact = filter (not . null)

toEvent :: String -> GuardId -> Event
toEvent "Guard" id = Begin id
toEvent "wakes" _ = WakeUp
toEvent "falls" _ = FallAsleep

toRecord :: String -> Record
toRecord = parse . compact . splitOnDelimiters

parse :: [String] -> Record
parse (year:month:day:hour:minute:action:id:_) = Record event timestamp
  where
  event :: Event
  event = toEvent action (read id)
  timestamp :: Timestamp
  timestamp = Timestamp (read year) (read month) (read day) (read hour) (read minute)

readLines :: String -> IO [String]
readLines = fmap lines . readFile

toTimeLog :: [Minute] -> TimeLog
toTimeLog minutes = M.fromListWith (+) [(minute, 1) | minute <- minutes]

minutesForGuard :: [Record] -> (GuardId, [Minute])
minutesForGuard ((Record (Begin id) _):xs) = (id, extractMinutes xs)

extractMinutes :: [Record] -> [Minute]
extractMinutes [] = []
extractMinutes ((Record FallAsleep asleep):(Record WakeUp awake):xs) = [minute asleep..(minute awake) - 1] ++ extractMinutes xs

beginningOfShift :: Event -> Bool
beginningOfShift (Begin _) = True
beginningOfShift _  = False

toTimeLogMap :: [Record] -> Map GuardId TimeLog
toTimeLogMap = M.map toTimeLog . M.fromListWith (++) . map minutesForGuard . splitRecord

splitRecord :: [Record] -> [[Record]]
splitRecord = split (keepDelimsL (dropInitBlank (whenElt (beginningOfShift . event))))

calculateResultBy :: ([Int] -> Int) -> [Record] -> Int
calculateResultBy function records = (fst (maximumBy (comparing snd) (M.toList (snd  worstGuard)))) * (fst worstGuard)
  where
  timeLogMap :: Map GuardId TimeLog
  timeLogMap = (toTimeLogMap . sort) records
  worstGuard :: (GuardId, TimeLog)
  worstGuard = maximumBy (comparing (function . M.elems . snd)) (filter (not . null . snd) (M.toList timeLogMap))

main :: IO ()
main = do
  records <- fmap (map toRecord) (readLines "input")
  print $ calculateResultBy sum records
  print $ calculateResultBy maximum records
