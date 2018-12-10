-- https://adventofcode.com/2018/day/7

{-# LANGUAGE TupleSections #-}

module TheSumOfItsParts where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char (ord)
import Data.List (sort, unfoldr)

type Node = Char
type Edge = (Node, Node)
type Graph = Map Node (Set Node)

readLines :: String -> IO [String]
readLines = fmap lines . readFile

firstNode :: String -> Node
firstNode = (!! 5)

secondNode :: String -> Node
secondNode = (!! 36)

extractEdge :: String -> Edge
extractEdge line = (secondNode line, firstNode line)

edgesToGraph :: [Edge] -> Graph
edgesToGraph = Map.fromListWith Set.union . (toGraph =<<)
  where
  toGraph :: Edge -> [(Node, Set Node)]
  toGraph edge = [(fst edge, Set.singleton (snd edge)), (snd edge, Set.empty)]

traverseGraph :: Graph -> [Node]
traverseGraph graph = case Map.lookupMin (compact graph) of
  Just (node, _) -> node : traverseGraph (Map.map (Set.delete node) (Map.delete node graph))
  _ | Map.null graph -> []
  where
  compact :: Graph -> Graph
  compact = Map.filter Set.null

parse :: String -> Map Char (Set Char)
parse input = Map.fromListWith Set.union $ concat
  [[(line !! 5, Set.empty), (line !! 36, Set.singleton $ line !! 5)] | line <- lines input]

simultaneousTraverse :: Int -> Int -> Graph -> Int
simultaneousTraverse workersCount stepDuration = last . unfoldr calculateTime . ([],)
  where
  duration :: Node -> Int
  duration = (+ 1) . subtract (ord 'A') . ord
  calculateTime :: ([(Int, Node)], Graph) -> Maybe (Int, ([(Int, Node)], Graph))
  calculateTime ([], graph)
    | graph == Map.empty = Nothing
    | otherwise = Just (0, sumTime 0 [] graph)
  calculateTime ((time, node):done, graph) = Just (time, sumTime time done (Map.map (Set.delete node) (Map.delete node graph)))
  sumTime :: Int -> [(Int, Node)] -> Graph -> ([(Int, Node)], Graph)
  sumTime time done graph = (, graph) $ sort $ done ++ take (workersCount - length done)
    [(time + stepDuration + duration node, node) | (node, nodes) <- Map.assocs graph, notElem node (map snd done), Set.null nodes]

main :: IO ()
main = do
  edges <- fmap (map extractEdge) (readLines "input")
  let graph = edgesToGraph edges
  print $ traverseGraph graph
  print $ simultaneousTraverse 5 60 graph
