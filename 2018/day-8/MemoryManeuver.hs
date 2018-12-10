-- https://adventofcode.com/2018/day/8

module MemoryManeuver where

import Data.List

type Metadata = Int
data Tree = Tree [Tree] [Metadata]
  deriving (Show, Eq)

readInts :: FilePath -> IO [Int]
readInts = fmap (map toInt) . fmap words . readFile

toInt :: String -> Int
toInt = read

toTree :: [Int] -> Tree
toTree = fst . toTree'

toTree' :: [Int] -> (Tree, [Int])
toTree' (x:y:xs) = (Tree trees (take y ys), (drop y ys))
  where
  (trees, ys) = subtrees x ([], xs)

subtrees :: Int -> ([Tree], [Int]) -> ([Tree], [Int])
subtrees nodesCount (trees, xs)
  | length trees == nodesCount = (trees, xs)
  | otherwise = subtrees nodesCount ((tree:trees), ys)
  where
  (tree, ys) = toTree' xs

sumMetadata :: Tree -> Metadata
sumMetadata (Tree [] metadata) = sum metadata
sumMetadata (Tree subtrees metadata) = sum metadata + sum (map sumMetadata subtrees)

treeValue :: Tree -> Metadata
treeValue (Tree [] metadata) = sum metadata
treeValue (Tree subtrees metadata) = sum (map metadataValue metadata)
  where
  metadataValue :: Metadata -> Metadata
  metadataValue x
    | x <= length subtrees = (map treeValue subtrees) !! (x - 1)
    | otherwise = 0

main :: IO ()
main = do
  tree <- fmap toTree (readInts "input")
  print $ sumMetadata tree
  print $ treeValue tree
