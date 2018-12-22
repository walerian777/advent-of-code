module ModeMaze where

type Coordinate = (Int, Int)
data RegionType = Rocky | Narrow | Wet
  deriving (Show, Eq)

caveDepth :: Int
caveDepth = 3066

caveMouth :: Coordinate
caveMouth = (0, 0)

target :: Coordinate
target = (13, 726)

geologicalIndex :: Coordinate -> Int
geologicalIndex (0, 0) = 0
geologicalIndex (13, 726) = 0
geologicalIndex (x, 0) = x * 16807
geologicalIndex (0, y) = y * 48271
geologicalIndex (x, y) = (erosionLevel (pred x, y)) * (erosionLevel (x, pred y))

erosionLevel :: Coordinate -> Int
erosionLevel coord = mod (geologicalIndex coord + caveDepth) 20183

regionType :: Coordinate -> RegionType
regionType coord = case mod (erosionLevel coord) 3 of
  0 -> Rocky
  1 -> Wet
  2 -> Narrow

smallestRectangle :: [Coordinate]
smallestRectangle = [(x, y)
                    | x <- [0 .. fst target]
                    , y <- [0 .. snd target]
                    ]

risk :: Coordinate -> Int
risk coord = case regionType coord of
  Rocky -> 0
  Wet -> 1
  Narrow -> 2

totalRisk :: [Coordinate] -> Int
totalRisk area = sum (map risk area)

main :: IO ()
main = do
  print $ totalRisk smallestRectangle
