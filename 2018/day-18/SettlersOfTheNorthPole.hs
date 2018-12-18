-- https://adventofcode.com/2018/day/18

{-# LANGUAGE FlexibleContexts #-}

module SettlersOfTheNorthPole where

import Data.Array.Unboxed (IArray, UArray, (!), (//), assocs, elems, listArray)
import Data.Map.Strict (empty, insertLookupWithKey)

type Coordinate = (Int, Int)
type Acre = (Coordinate, Char)

toArea :: [String] -> UArray Coordinate Char
toArea input = listArray ((0, 0), (49, 49)) (concat input)

ground = '.' :: Char
tree = '|' :: Char
lumberyard = '#' :: Char

morph :: (IArray a Char) => Acre -> a Coordinate Char -> Acre
morph acre@(coord, acreType) area
  | acreType == ground
    && scanArea surrounding tree >= 3 = (coord, tree)
  | acreType == tree
    && scanArea surrounding lumberyard >= 3 = (coord, lumberyard)
  | acreType == lumberyard
    && scanArea surrounding tree >= 1
    && scanArea surrounding lumberyard >= 1 = (coord, lumberyard)
  | acreType == lumberyard = (coord, ground)
  | otherwise = acre
    where
    surrounding = (area !) <$> (neighbours coord)
    neighbours :: Coordinate -> [Coordinate]
    neighbours (x, y) = [(k, l)
                        | k <- [x - 1 .. x + 1]
                        , l <- [y - 1 .. y + 1]
                        , (k, l) /= (x, y)
                        , k >= 0, l >= 0
                        , k < 50, l < 50
                        ]

scanArea :: [Char]-> Char -> Int
scanArea area acreType = length (filter (== acreType) area)

epoch :: (IArray a Char) => a Coordinate Char -> a Coordinate Char
epoch area = area // do
    acre@(coords, current) <- assocs area
    let acreType = snd $ morph acre area
    return (coords, acreType)

resourceValue :: (IArray a Char) => a Coordinate Char -> Int
resourceValue area = trees * lumberyards
  where
  trees = scanArea (elems area) tree :: Int
  lumberyards =  scanArea (elems area) lumberyard :: Int

loopedEpoch :: (IArray a Char, Ord (a Coordinate Char)) =>
  Int -> a Coordinate Char -> a Coordinate Char
loopedEpoch minutes area = iterate empty 0 minutes area
  where
  iterate lookup done epochs nextArea
    | done == epochs = nextArea
    | otherwise = iterate lookup' (done + 1) epochs' $ epoch nextArea
      where
      (hit, lookup') = insertLookupWithKey (const (const id)) nextArea done lookup
      epochs' = maybe minutes (((done + 1) +) . ((epochs - done - 1) `mod`) . (-) done) hit

main :: IO ()
main = do
  input <- fmap lines (readFile "input")
  let area = toArea input
  print $ resourceValue (loopedEpoch 10 area)
  print $ resourceValue (loopedEpoch 1000000000 area)
