-- https://adventofcode.com/2018/day/20

module RegularMap where

import Data.Map (Map, notMember, (!), insert, elems)
import qualified Data.Map.Strict as Map
import Data.Stack
import Data.Bool (bool)
import Data.List (filter)

type Coordinate = (Int, Int)
type Distance = Int
type Grid = Map Coordinate Distance
type Distances = Stack (Coordinate, Distance)

toDirections :: String -> String
toDirections = takeWhile (/= '$') . drop 1

directionValue :: Char -> Char -> Int
directionValue x y = bool 0 1 (x == y)

generateGrid :: String -> Grid
generateGrid directions = generateGrid' directions Map.empty stackNew (0, 0) 0

generateGrid' :: String -> Grid -> Distances -> Coordinate -> Distance -> Grid
generateGrid' [] grid _ _ _ = grid
generateGrid' (d:ds) grid stack (x, y) distance
  | d == '(' = generateGrid' ds grid pushStack (x, y) distance
  | d == ')' = generateGrid' ds grid popStack popCoord popDist
  | d == '|' = generateGrid' ds grid stack peekCoord peekDist
  | notMember (newX, newY) grid || newDistance < grid ! (newX, newY)
    = generateGrid' ds newGrid stack (newX, newY) newDistance
  | otherwise = generateGrid' ds grid stack (newX, newY) newDistance
  where
  Just (popStack, (popCoord, popDist)) = stackPop stack
  Just (peekCoord, peekDist) = stackPeek stack
  pushStack :: Distances
  pushStack = stackPush stack ((x, y), distance)
  newX :: Int
  newX = x + directionValue d 'E' - directionValue d 'W'
  newY :: Int
  newY = y + directionValue d 'S' - directionValue d 'N'
  newGrid :: Grid
  newGrid = insert (newX, newY) newDistance grid
  newDistance :: Distance
  newDistance = succ distance

main :: IO ()
main = do
  input <- readFile "input"
  let directions = toDirections input
  let grid = generateGrid directions
  print $ maximum (elems grid)
  print $ length (filter (>= 1000) (elems grid))
