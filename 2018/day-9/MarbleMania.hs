-- https://adventofcode.com/2018/day/9

module MarbleMania where

import Control.Monad(liftM2)
import Data.List.Split(splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Score = Int
type Tally = Map Int Int
type Ring = Seq Score

readInput :: FilePath -> IO [String]
readInput = fmap (splitOn " ") . readFile

extractRules :: [String] -> (Int, Int)
extractRules = liftM2 (,) extractPlayersCount extractLastMarbleValue
  where
  extractPlayersCount :: [String] -> Int
  extractPlayersCount = read . head
  extractLastMarbleValue :: [String] -> Int
  extractLastMarbleValue = read . (!! 6)

play :: Int -> Score -> Score
play = play' Map.empty (Seq.singleton 0) 1

play' :: Tally -> Ring -> Int -> Score -> Int -> Score
play' tally ring score players goal
  | score >= goal = maximum tally
  | rem score 23 == 0 = case insert (-7) ring of
    current Seq.:<| ring' -> play' (Map.insertWith (+) (rem score players) (score + current) tally) ring' (score + 1) players goal
  | otherwise = play' tally (score Seq.<| insert 2 ring) (score + 1) players goal

insert :: Score -> Ring -> Ring
insert x ring
  | ring == Seq.empty = ring
  | otherwise = ccw <> cw
  where
  (cw, ccw) = Seq.splitAt (mod x (Seq.length ring)) ring

main :: IO ()
main = do
  (playersCount, lastMarbleValue) <- fmap extractRules (readInput "input")
  print $ play playersCount lastMarbleValue
  print $ play playersCount (lastMarbleValue * 100)
