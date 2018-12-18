-- https://adventofcode.com/2018/day/17
{-# LANGUAGE TupleSections, ViewPatterns, FlexibleContexts #-}

module ReservoirResearch where

import Data.List.Split (splitOn)
import Data.Array.IArray (Array, accumArray, elems)
import Data.Array.ST (MArray, getBounds, thaw, readArray, runSTArray, writeArray)
import Data.Semigroup (Max(Max), Min(Min), sconcat)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Control.Monad (filterM)
import Control.Monad.Loops (andM, dropWhileM, takeWhileM)
import Control.Arrow ((&&&), (***))
import Data.List.NonEmpty (nonEmpty)
import Data.Bool (bool)

type Coordinate = (Int, Int)
type Grid = Array Coordinate Tile

data Tile = Sand | Clay | StagnantWater | FlowingWater
  deriving (Eq)

instance Show Tile where
  show Sand = "."
  show Clay = "#"
  show StagnantWater = "~"
  show FlowingWater = "|"

toCoordinate :: String -> [Coordinate]
toCoordinate (c:_:cs) = case c of
  'x' -> [(x, y) | y <- [k..l]]
  'y' -> [(y, x) | y <- [k..l]]
  where
  x :: Int
  x = read (takeWhile (/= ',') cs)
  (k:l:_) = map read (splitOn ".." (last (splitOn "=" cs)))

toGrid :: [String] -> Grid
toGrid = toTiles . (toCoordinate =<<)

toTiles :: [Coordinate] -> Grid
toTiles xs = accumArray (flip const) Sand ((minY, minX), (maxY, maxX)) ((, Clay) <$> xs)
  where
  ((Min minY, Min minX), (Max maxY, Max maxX)) = case nonEmpty xs of
    Just xs' -> sconcat (((Min *** Min . pred) &&& (Max *** Max . succ)) <$> xs')

ranges :: [Int] -> [Coordinate]
ranges = ranges' Nothing

ranges' :: Maybe Coordinate -> [Int] -> [Coordinate]
ranges' coord [] = maybeToList coord
ranges' Nothing (x:xs) = ranges' (Just (x, x)) xs
ranges' (Just (l, r)) (x:xs)
  | r + 1 < x = (l, r) : ranges' (Just (x, x)) xs
  | otherwise = ranges' (Just (l, max r x)) xs

start :: (MArray a Tile m) => a (Int, Int) Tile -> m Bool
start scene = do
  ((minY, minX), (maxY, maxX)) <- getBounds scene
  let flow y (x0, x1)
        | y > maxY = return False
        | otherwise = do
          sandTiles <- filterM (\x -> (== Sand) <$> readArray scene (y, x)) [x0..x1]
          clayTiles0 <- andM [(/= FlowingWater) <$> readArray scene (y, x) | x <- [x0..x1]]
          clayTiles1 <- and <$> mapM (fill y) (ranges sandTiles)
          return (clayTiles0 && clayTiles1)
      fill y (x0, x1) = do
        clayTiles <- flow (y + 1) (x0, x1)
        if not clayTiles
        then False <$ sequence_ [writeArray scene (y, x) FlowingWater | x <- [x0..x1]]
        else do
        let isSand x = (== Sand) <$> readArray scene (y, x)
            isFillable x =
              if y < maxY
              then (`notElem` [Sand, FlowingWater]) <$> readArray scene (y + 1, x)
              else return False
        lefts <- takeWhileM isSand [x0 - 1, x0 - 2..minX]
        rights <- takeWhileM isSand [x1 + 1, x1 + 2..maxX]
        l <- fromMaybe (last (x0:lefts)) . listToMaybe <$> dropWhileM isFillable lefts
        r <- fromMaybe (last (x1:rights)) . listToMaybe <$> dropWhileM isFillable rights
        if l >= x0 && r <= x1
        then True <$ sequence_ [writeArray scene (y, x) StagnantWater | x <- [x0..x1]]
        else do
          sequence_ [writeArray scene (y, x) Clay | x <- [x0..x1]]
          clayTilesL <- fill y (l, x0 - 1)
          clayTilesR <- fill y (x1 + 1, r)
          let clayTiles'@(bool FlowingWater StagnantWater -> tile) = clayTilesL && clayTilesR
          clayTiles' <$ sequence_ [writeArray scene (y, x) tile | x <- [l..r]]
  flow minY (500, 500)

filterWaterTiles :: Grid -> [Tile]
filterWaterTiles = filter (`elem` [StagnantWater, FlowingWater]) . elems

filterStagnantTiles :: Grid -> [Tile]
filterStagnantTiles = filter (== StagnantWater) . elems

main :: IO ()
main = do
  input <- fmap lines (readFile "input")
  let grid = runSTArray (thaw (toGrid input) >>= (\x -> x <$ start x))
  print $ length (filterWaterTiles grid)
  print $ length (filterStagnantTiles grid)
