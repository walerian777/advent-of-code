-- https://adventofcode.com/2018/day/14

{-# LANGUAGE ViewPatterns #-}

module ChocolateCharts where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.List (isPrefixOf, tails)

type Score = Int

data Board = Board { firstElf :: Int
                   , secondElf :: Int
                   , scores :: Seq Score
                   }

listToString :: [Int] -> String
listToString = concat . (map toString)

toString :: Int -> String
toString = show

recipesCount :: Int
recipesCount = 990941

combineRecipes :: Score -> Score -> [Score]
combineRecipes x y = case divMod (x + y) 10 of
  (0, z) -> [z]
  (w, z) -> [w, z]

process :: Board -> (Board, [Score])
process (Board firstElf secondElf scores) = (Board newFirstElf newSecondElf newScoreSeq, newScores)
  where
  x :: Score
  x = Seq.index scores firstElf
  y :: Score
  y = Seq.index scores secondElf
  newScores :: [Score]
  newScores = combineRecipes x y
  newScoreSeq :: Seq Score
  newScoreSeq = scores <> Seq.fromList newScores
  newFirstElf :: Int
  newFirstElf = mod (firstElf + x + 1) (length newScoreSeq)
  newSecondElf :: Int
  newSecondElf = mod (secondElf + y + 1) (length newScoreSeq)

findScore :: String
findScore = listToString ([3, 7] ++ find (Board 0 1 (Seq.fromList [3,7])))
  where
  find :: Board -> [Score]
  find (process->(board, scores)) = scores ++ find board

substr :: String -> String -> Int
substr = (length .) . (. tails) . takeWhile . (not .) . isPrefixOf

main :: IO ()
main = do
  print $ take 10 (drop recipesCount findScore)
  print $ substr (toString recipesCount) findScore
