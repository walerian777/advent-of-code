{-# LANGUAGE FlexibleContexts, NamedFieldPuns, RecordWildCards, TransformListComp, TupleSections, TypeApplications #-}

module ImmuneSystemSimulator where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow ((&&&))
import Data.List (foldl', mapAccumL, sortOn, find)
import Data.Map.Strict (alter, assocs, fromList)
import Data.Maybe (catMaybes, listToMaybe, fromJust)
import Data.Ord (Down(..))
import Data.Set (Set, empty, insert, member, notMember)
import GHC.Exts (sortWith)
import Prelude hiding (round)

data Side = ImmuneSystem | Infection
  deriving (Show, Eq, Ord)

data AttackType = Slashing | Cold | Fire | Bludgeoning | Radiation
  deriving (Show, Eq, Ord)

data Key = Key { side :: Side, n :: Int }
  deriving (Eq, Ord)

data Group = Group { count :: Int
                   , hitPoints :: Int
                   , attackType :: AttackType
                   , attackPoints :: Int
                   , weak :: [AttackType]
                   , immune :: [AttackType]
                   , initiative :: Int
                   } deriving (Show, Eq)

type Reindeer = [(Key, Group)]

groups :: [(Key, Group)]
groups = [
  (
    Key {side = ImmuneSystem, n = 1},
    Group {count = 698, hitPoints = 10286, weak = [], immune = [], attackPoints = 133, attackType = Fire, initiative = 9}
  ),
  (
    Key {side = ImmuneSystem, n = 2},
    Group {count = 6846, hitPoints = 2773, weak = [Slashing,Cold], immune = [], attackPoints = 4, attackType = Slashing, initiative = 14}
  ),
  (
    Key {side = ImmuneSystem, n = 3},
    Group {count = 105, hitPoints = 6988, weak = [Bludgeoning], immune = [Radiation], attackPoints = 616, attackType = Radiation, initiative = 17}
  ),
  (
    Key {side = ImmuneSystem, n = 4},
    Group {count = 5615, hitPoints = 7914, weak = [Bludgeoning], immune = [], attackPoints = 13, attackType = Radiation, initiative = 20}
  ),
  (
    Key {side = ImmuneSystem, n = 5},
    Group {count = 1021, hitPoints = 10433, weak = [Cold], immune = [Slashing,Bludgeoning], attackPoints = 86, attackType = Bludgeoning, initiative = 12}
  ),
  (
    Key {side = ImmuneSystem, n = 6},
    Group {count = 6099, hitPoints = 11578, weak = [], immune = [], attackPoints = 15, attackType = Bludgeoning, initiative = 13}
  ),
  (
    Key {side = ImmuneSystem, n = 7},
    Group {count = 82, hitPoints = 1930, weak = [Bludgeoning], immune = [Cold], attackPoints = 179, attackType = Bludgeoning, initiative = 5}
  ),
  (
    Key {side = ImmuneSystem, n = 8},
    Group {count = 2223, hitPoints = 9442, weak = [], immune = [Bludgeoning], attackPoints = 38, attackType = Cold, initiative = 19}
  ),
  (
    Key {side = ImmuneSystem, n = 9},
    Group {count = 140, hitPoints = 7594, weak = [Radiation], immune = [], attackPoints = 452, attackType = Fire, initiative = 8}
  ),
  (
    Key {side = ImmuneSystem, n = 10},
    Group {count = 3057, hitPoints = 3871, weak = [Bludgeoning], immune = [], attackPoints = 11, attackType = Radiation, initiative = 16}
  ),
  (
    Key {side = Infection, n = 1},
    Group {count = 263, hitPoints = 48098, weak = [Slashing], immune = [Radiation], attackPoints = 293, attackType = Bludgeoning, initiative = 2}
  ),
  (
    Key {side = Infection, n = 2},
    Group {count = 111, hitPoints = 9893, weak = [], immune = [Slashing], attackPoints = 171, attackType = Fire, initiative = 18}
  ),
  (
    Key {side = Infection, n = 3},
    Group {count = 2790, hitPoints = 36205, weak = [], immune = [], attackPoints = 25, attackType = Cold, initiative = 4}
  ),
  (
    Key {side = Infection, n = 4},
    Group {count = 3325, hitPoints = 46479, weak = [Slashing], immune = [], attackPoints = 27, attackType = Radiation, initiative = 1}
  ),
  (
    Key {side = Infection, n = 5},
    Group {count = 3593, hitPoints = 6461, weak = [Fire,Slashing], immune = [], attackPoints = 3, attackType = Radiation, initiative = 15}
  ),
  (
    Key {side = Infection, n = 6},
    Group {count = 2925, hitPoints = 13553, weak = [Cold,Bludgeoning], immune = [Fire], attackPoints = 8, attackType = Cold, initiative = 10}
  ),
  (
    Key {side = Infection, n = 7},
    Group {count = 262, hitPoints = 43260, weak = [Cold], immune = [], attackPoints = 327, attackType = Radiation, initiative = 6}
  ),
  (
    Key {side = Infection, n = 8},
    Group {count = 4228, hitPoints = 24924, weak = [Radiation,Fire], immune = [Cold,Bludgeoning], attackPoints = 11, attackType = Cold, initiative = 11}
  ),
  (
    Key {side = Infection, n = 9},
    Group {count = 689, hitPoints = 42315, weak = [Cold,Slashing], immune = [], attackPoints = 116, attackType = Fire, initiative = 7}
  ),
  (
    Key {side = Infection, n = 10},
    Group {count = 2649, hitPoints = 37977, weak = [Radiation], immune = [], attackPoints = 24, attackType = Cold, initiative = 3}
  )]

round :: Reindeer -> Reindeer
round groups = assocs (foldl' attack (fromList groups) attacks)
  where
  attacks = map snd $ sortOn (Down . fst) $ catMaybes $ snd $ mapAccumL buildAttack empty
    [ (key, effectivePower, attackType, initiative)
    | (key, Group { count, attackPoints, attackType, initiative }) <- groups
    , let effectivePower = count * attackPoints
    , then sortWith by Down (effectivePower, initiative)
    ]
  buildAttack used (src, effectivePower, srcType, srcInitiative) =
    maybe (used, Nothing) ((`insert` used) &&& Just . (srcInitiative,) . (src,)) $
    listToMaybe
      [ dst
      | (dst, Group {count, attackPoints, weak, immune, initiative}) <- groups
      , side src /= side dst
      , notMember dst used
      , notElem srcType immune
      , let damage = (if elem srcType weak then 2 else 1) * effectivePower
      , then sortWith by Down (damage, count * attackPoints, initiative)
      ]
  attack groups' (attacker, defender)
    | Just Group { count, attackPoints, attackType } <- Map.lookup attacker groups'
    = alter (strike attackType (count * attackPoints)) defender groups'
    | otherwise = groups'
  strike attackType effectivePower (Just defender@Group { count, hitPoints, weak })
    | count > killed = Just defender {count = count - killed}
    | otherwise = Nothing
    where killed = (if elem attackType weak then 2 else 1) * div effectivePower hitPoints

fight :: Reindeer -> Reindeer
fight = fight' empty

fight' :: Set [(Side, Int, Int)] -> Reindeer -> Reindeer
fight' seen groups
  | member key seen = groups
  | otherwise = fight' (insert key seen) (round groups)
  where key = [(side, n, count) | (Key { side, n }, Group { count }) <- groups]

countSurvivors :: Reindeer -> Int
countSurvivors = sum . fmap (count . snd)

boostedFight :: Reindeer -> Maybe Reindeer
boostedFight groups
  = find (all $ (== ImmuneSystem) . side . fst)
    [ fight
        [ (k, if side == ImmuneSystem then group { attackPoints = attackPoints + boost } else group)
        | (k@Key { side }, group@Group { attackPoints }) <- groups
        ]
    | boost <- [0..]
    ]

main :: IO ()
main = do
  print $ countSurvivors (fight groups)
  print $ countSurvivors (fromJust (boostedFight groups))
