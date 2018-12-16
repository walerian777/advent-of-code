-- https://adventofcode.com/2018/day/15

{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module BeverageBandits where

import Data.Array.Unboxed (UArray, (//), (!))
import qualified Data.Array.Unboxed as UArray
import Data.List (sort, sortBy)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)

type Position = (Int, Int)
type Cave = UArray Position Char
type Combat = (Creature, Creature)
type Move = ((Int, Position), Position)

data Race = Goblin | Elf
  deriving (Show, Eq)

data Tile = Tile Creature | Wall | Grounds
  deriving (Show)

data Creature = Creature { race :: Race
                         , position :: Position
                         , power :: Int
                         , hitpoints :: Int
                         } deriving (Show, Eq)

data Battle = Battle { cave :: Cave
                     , creatures :: [Creature]
                     , idleCreatures :: [Creature]
                     , rounds :: Int
                     , finish :: Bool
                     } deriving (Show)

instance Ord Creature where
  compare Creature { position = (x, y) } Creature { position = (w, z) }
    | x < w = LT
    | x > w = GT
    | y < z = LT
    | y > z = GT
    | otherwise = EQ

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

toTile :: Char -> Tile
toTile '#' = Wall
toTile '.' = Grounds
toTile 'G' = Tile (Creature Goblin (0, 0) 3 200)
toTile 'E' = Tile (Creature Elf (0, 0) 3 200)

toCreature :: (Tile, Position) -> Creature
toCreature ((Tile c@(Creature {..})), x) =  c { position = x }

isCreature :: Tile -> Bool
isCreature (Tile Creature {..}) = True
isCreature _ = False

prepareBattle :: [String] -> Battle
prepareBattle input = Battle cave creatures [] 0 False
  where
  grid :: [Position]
  grid = [(x, y) | x <- [0..31], y <- [0..31]]
  tiles :: [Tile]
  tiles = map toTile (concat input)
  creatures :: [Creature]
  creatures = map toCreature (filter (isCreature . fst) (zip tiles grid))
  cave :: Cave
  cave = UArray.array ((0, 0), (31, 31)) (zip grid (concat input))

sumHitpoints :: Battle -> Int
sumHitpoints Battle { creatures, idleCreatures } = sum (map hitpoints (creatures ++ idleCreatures))

fight :: Battle -> Int
fight battle@(Battle { rounds, finish = True }) = rounds * sumHitpoints battle
fight battle@(Battle { creatures = [] }) = fight (nextRound battle)
fight battle = case nextCombat of
  Just combat -> fight (processCombat (nextBattle, combat))
  Nothing -> fight nextBattle
  where
  (nextBattle, nextCombat) = nextTurn battle

nextRound :: Battle -> Battle
nextRound battle@(Battle { creatures, idleCreatures, rounds })
  = battle { creatures = sort idleCreatures, idleCreatures = [], rounds = rounds + 1 }

processCombat :: (Battle, Combat) -> Battle
processCombat (battle@(Battle { cave, creatures, idleCreatures }),
  (Creature { power }, defender@(Creature { position, hitpoints })))
    | power >= hitpoints
      = battle { cave = newCave, creatures = filtered creatures, idleCreatures = filtered idleCreatures }
    | otherwise = battle { creatures = mapped creatures, idleCreatures = mapped idleCreatures }
    where
    newCave :: Cave
    newCave = cave // [(position, '.')]
    filtered :: [Creature] -> [Creature]
    filtered = filter (/= defender)
    mapped :: [Creature] -> [Creature]
    mapped = map dealDamage
    dealDamage :: Creature -> Creature
    dealDamage creature
      | creature == defender = creature { hitpoints = hitpoints - power }
      | otherwise = creature

nextTurn :: Battle -> (Battle, Maybe Combat)
nextTurn battle@(Battle cave (c@(Creature cRace (x, y) hitpoints power):cs) idleCreatures _ _)
  | null targets = (battle { creatures = cs, idleCreatures = (c:idleCreatures), finish = True }, Nothing)
  | (not . null) closeTargets = (battle {creatures = cs, idleCreatures = (c:idleCreatures) }, closeCombat)
  | null enemyMoves = (battle {creatures = cs, idleCreatures = (c:idleCreatures) }, Nothing)
  | (not . null) remoteTargets = (battle {creatures = cs, idleCreatures = (movedC:idleCreatures) }, remoteCombat)
  | otherwise = (battle {creatures = cs, idleCreatures = (movedC:idleCreatures) }, Nothing)
  where
  targets :: [Creature]
  targets = filter ((/= cRace) . race) (cs ++ idleCreatures)
  closeTargets :: [Creature]
  closeTargets = sortBy sortTargets (filter ((`elem` moves) . position) targets)
  moves :: [Position]
  moves = possibleMoves (x, y)
  closeCombat :: Maybe Combat
  closeCombat = Just (c, head closeTargets)
  enemyMoves = sortBy sortMoves (concat (map findMoves targets))
  findMoves = (prepareMoves cave allowedMoves . position)
  allowedMoves = filter (((== '.') . (cave !))) moves
  remoteTargets :: [Creature]
  remoteTargets = sortBy sortTargets (filter ((`elem` (possibleMoves step)) . position) targets)
  ((_, step), _) = head enemyMoves
  movedC = c { position = step }
  remoteCombat = Just (c, head remoteTargets)

prepareMoves :: Cave -> [Position] -> Position -> [Move]
prepareMoves cave moves target = catMaybes (map distance moves)
  where
  distance = calculateDistance cave Set.empty 1 target

calculateDistance :: Cave -> Set Position -> Int -> Position -> Position -> Maybe Move
calculateDistance cave positions distance target start
  | (cave ! start) /= '.' = Nothing
  | Set.null positions = calculateDistance cave (Set.insert start positions) distance target start
  | elem target nextNeighbors = Just ((distance, start), target)
  | positions == nextPositions = Nothing
  | otherwise = calculateDistance cave nextPositions (distance + 1) target start
  where 
    nextPositions = insertList positions $ filter ((=='.') . (cave !)) $  nextNeighbors
    nextNeighbors = filter (not . (`Set.member` positions)) . possibleMoves =<< (Set.toList positions)
 
insertList :: Set Position -> [Position] -> Set Position
insertList set [] = set
insertList set (x:xs) = insertList (Set.insert x set) xs

sortTargets :: Creature -> Creature -> Ordering
sortTargets Creature { position = (x, y), hitpoints = hpA } Creature { position = (w, z), hitpoints = hpB }
  | hpA < hpB = LT
  | hpA > hpB = GT
  | x < w = LT
  | x > w = GT
  | y < z = LT
  | y > z = GT

sortMoves :: Move -> Move -> Ordering
sortMoves ((dA, (xA, yA)), (wA, zA)) ((dB, (xB, yB)), (wB, zB))
  | dA < dB = LT
  | dA > dB = GT
  | wA < wB = LT
  | wA > wB = GT
  | zA < zB = LT
  | zA > zB = GT
  | xA < xB = LT
  | xA > xB = GT
  | yA < yB = LT
  | yA > yB = GT

possibleMoves :: Position -> [Position]
possibleMoves (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

main :: IO ()
main = do
  input <- readLines "input"
  let battle = prepareBattle input
  print $ fight battle
