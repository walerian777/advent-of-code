-- https://adventofcode.com/2018/day/3

module NoMatterHowYouSliceIt where

import Data.List.Split (splitOneOf)
import Data.List (find)
import Data.Ix (range)
import Data.Map (Map)
import qualified Data.Map as M

type ID = Int
type Point = (Int, Int)
type AreaMap = Map Point Int

data Claim = Claim { id :: ID
                   , position :: Point
                   , width :: Int
                   , height :: Int
                   } deriving (Show)

splitOnDelimiters :: String -> [String]
splitOnDelimiters = splitOneOf [' ', '#', '@', ',', ':', 'x']

compact :: [String] -> [String]
compact = filter (not . null)

toClaim :: String -> Claim
toClaim = parse . map read . compact . splitOnDelimiters
  where parse [id, x, y, width, height] = Claim id (x, y) width height

calculateArea :: Claim -> [Point]
calculateArea (Claim _ (x, y) width height) = range ((x, y), (endX, endY))
  where
  endX = x + width - 1
  endY = y + height - 1

claimedFabric :: [Claim] -> AreaMap
claimedFabric claims = M.fromListWith (+) [(area, 1) | area <- concatMap calculateArea claims]

countOverlaps :: [Claim] -> Int
countOverlaps = length . filter (> 1) . M.elems . claimedFabric

fabricDoesNotOverlap :: AreaMap -> Claim -> Bool
fabricDoesNotOverlap claimedArea claim = all (\x -> M.lookup x claimedArea == Just 1) (calculateArea claim)

findNotOverlapping :: [Claim] -> Maybe Claim
findNotOverlapping = find =<< fabricDoesNotOverlap . claimedFabric

readLines :: String -> IO [String]
readLines = fmap lines . readFile

main :: IO ()
main = do
  claims <- fmap (map toClaim) (readLines "input")
  print $ countOverlaps claims
  print $ findNotOverlapping claims
