-- https://adventofcode.com/2018/day/7

module TheSumOfItsParts where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

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

main :: IO ()
main = do
  edges <- fmap (map extractEdge) (readLines "input")
  print $ traverseGraph (edgesToGraph edges)
