-- https://adventofcode.com/2018/day/13

{-# LANGUAGE NamedFieldPuns #-}

module MineCartMadness where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Utils (hasAny, uniq)
import Data.List ((\\), sort)
import Data.Maybe (isJust, fromJust)

type Position = (Int, Int)
type Track = Map Position Char

data Direction = North | West | South | East
                 deriving (Show, Eq)

data Turn = TurnRight | TurnLeft | Straight

data Cart = Cart { position :: Position
                 , direction :: Direction
                 , turnsCount :: Int
                 } deriving (Show, Eq)

data Circuit = Circuit { track :: Track
                       , carts :: [Cart]
                       , idleCarts :: [Cart]
                       } deriving (Show)

instance Ord Cart where
  compare (Cart (x, y) _ _) (Cart (w, z) _ _)
    | x < w = LT
    | x > w = GT
    | y < z = LT
    | y > z = GT
    | otherwise = EQ

toCircuit :: [String] -> Circuit
toCircuit input = Circuit track carts []
  where
  grid :: [Position]
  grid = [(x, y) | x <- [0..(length input - 1)], y <- [0..(length (head input) - 1)]]
  positions :: [(Position, Char)]
  positions = zip grid (filter ('\n' /=) (concat input))
  carts :: [Cart]
  carts = map toCart (filter (isCart . snd) positions)
  track :: Track
  track = Map.fromList (map toTrack positions)
  isCart :: Char -> Bool
  isCart x = hasAny "^<>v" [x]
  toCart :: (Position, Char) -> Cart
  toCart (position, '^') = Cart position North 0
  toCart (position, '<') = Cart position West 0
  toCart (position, 'v') = Cart position South 0
  toCart (position, '>') = Cart position East 0
  toTrack :: (Position, Char) -> (Position, Char)
  toTrack (x, '^') = (x, '|')
  toTrack (x, '<') = (x, '-')
  toTrack (x, 'v') = (x, '|')
  toTrack (x, '>') = (x, '-')
  toTrack (x, y) = (x, y)

move :: Cart -> Cart
move cart@(Cart { direction = North, position = (x, y) }) = cart { position = (x - 1, y) }
move cart@(Cart { direction = West, position = (x, y) }) = cart { position = (x, y - 1) }
move cart@(Cart { direction = South, position = (x, y) }) = cart { position = (x + 1, y) }
move cart@(Cart { direction = East, position = (x, y) }) = cart { position = (x, y + 1) }

cross :: Cart -> Cart
cross cart@(Cart { direction, turnsCount }) = cart { direction = newDirection, turnsCount = turnsCount + 1 }
  where
  newDirection :: Direction
  newDirection = turn nextTurn direction
  nextTurn :: Turn
  nextTurn = case mod turnsCount 3 of
    0 -> TurnLeft
    1 -> Straight
    2 -> TurnRight

turnLeft :: Cart -> Cart
turnLeft cart@(Cart { direction }) = cart { direction = newDirection } 
  where
  newDirection :: Direction
  newDirection = case direction of
    North -> East
    West -> South
    South -> West
    East -> North

turnRight :: Cart -> Cart
turnRight cart@(Cart { direction }) = cart { direction = newDirection } 
  where
  newDirection :: Direction
  newDirection = case direction of
    North -> West
    West -> North
    South -> East
    East -> South

turn :: Turn -> Direction -> Direction
turn TurnRight North = East
turn TurnRight West = North
turn TurnRight South = West
turn TurnRight East = South
turn TurnLeft North = West
turn TurnLeft West = South
turn TurnLeft South = East
turn TurnLeft East = North
turn Straight direction = direction

nextMove :: Circuit -> Circuit
nextMove circuit@(Circuit _ [] idleCarts) = circuit { carts = sortedCarts, idleCarts = [] }
  where
  sortedCarts :: [Cart]
  sortedCarts = sort idleCarts
nextMove circuit@(Circuit track ((cart):xs) ys) = circuit { carts = xs, idleCarts = (newCart:ys) }
  where
  tile :: Maybe Char
  tile = Map.lookup (position cart) track
  newCart :: Cart
  newCart = case tile of
    Just '+' -> move (cross cart)
    Just '/' -> move (turnLeft cart)
    Just '\\' -> move (turnRight cart)
    Just '|' -> move cart
    Just '-' -> move cart
    Nothing -> cart

firstCrash :: Circuit -> Position
firstCrash circuit@(Circuit { carts, idleCarts })
  | isJust crashPosition = fromJust crashPosition
  | otherwise = firstCrash (nextMove circuit)
  where
  crashPosition :: Maybe Position
  crashPosition = findCrash (carts ++ idleCarts)

findCrash :: [Cart] -> Maybe Position
findCrash carts = case positions \\ uniq positions of
  (x:_) -> Just x
  [] -> Nothing
  where
  positions :: [Position]
  positions = map position carts

main :: IO ()
main = do
  input <- fmap lines (readFile "input")
  let circuit = toCircuit input
  print $ firstCrash circuit
  print $ firstCrash circuit
