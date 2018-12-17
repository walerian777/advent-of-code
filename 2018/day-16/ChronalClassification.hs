-- https://adventofcode.com/2018/day/16

module ChronalClassification where

import Data.List.Split (splitOn)
import Data.List ((\\))
import Data.Bits ((.&.), (.|.))
import Data.Bool (bool)

type Register = (Int, Int, Int, Int)
type Elements = (Int, Int, Int)
type Op = (Int, Elements)

data Sample = Sample Register Op Register
  deriving (Show, Eq)

data Opcode = Addr | Addi
            | Mulr | Muli
            | Banr | Bani
            | Borr | Bori
            | Setr | Seti
            | Gtir | Gtri
            | Gtrr | Eqir
            | Eqri | Eqrr
            deriving (Show, Eq)

allOpcodes :: [Opcode]
allOpcodes = [Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr]

readInput :: FilePath -> IO [String]
readInput = fmap (splitOn "\n\n\n\n") . readFile

toInt :: String -> Int
toInt = read

toRegister :: String -> Register
toRegister str = (a, b, c, d)
  where
  [a, b, c, d] = map toInt (splitOn ", " ((drop 9 . takeWhile (/= ']')) str))

toOp :: String -> Op
toOp str = (op, (a, b, c))
  where
  [op, a, b, c] = map toInt (words str)

getReg :: Register -> Int -> Int
getReg (a, b, c, d) index = case index of
  0 -> a
  1 -> b
  2 -> c
  3 -> d

setReg :: Register -> Int -> Int -> Register
setReg (a, b, c, d) index x = case index of
  0 -> (x, b, c, d)
  1 -> (a, x, c, d)
  2 -> (a, b, x, d)
  3 -> (a, b, c, x)

process :: Opcode -> Register -> Op -> Register
process Addr reg (_, (a, b, c)) = setReg reg c $ (getReg reg a) + (getReg reg b)
process Addi reg (_, (a, b, c)) = setReg reg c $ (getReg reg a) + b
process Mulr reg (_, (a, b, c)) = setReg reg c $ (getReg reg a) * (getReg reg b)
process Muli reg (_, (a, b, c)) = setReg reg c $ (getReg reg a) * b
process Banr reg (_, (a, b, c)) = setReg reg c $ (getReg reg a) .&. (getReg reg b)
process Bani reg (_, (a, b, c)) = setReg reg c $ (getReg reg a) .&. b
process Borr reg (_, (a, b, c)) = setReg reg c $ (getReg reg a) .|. (getReg reg b)
process Bori reg (_, (a, b, c)) = setReg reg c $ (getReg reg a) .|. b
process Setr reg (_, (a, b, c)) = setReg reg c $ (getReg reg a)
process Seti reg (_, (a, b, c)) = setReg reg c $ a
process Gtir reg (_, (a, b, c)) = setReg reg c $ bool 0 1 $ a > (getReg reg b)
process Gtri reg (_, (a, b, c)) = setReg reg c $ bool 0 1 $ (getReg reg a) > b
process Gtrr reg (_, (a, b, c)) = setReg reg c $ bool 0 1 $ (getReg reg a) > (getReg reg b)
process Eqir reg (_, (a, b, c)) = setReg reg c $ bool 0 1 $ a == (getReg reg b)
process Eqri reg (_, (a, b, c)) = setReg reg c $ bool 0 1 $ (getReg reg a) == b
process Eqrr reg (_, (a, b, c)) = setReg reg c $ bool 0 1 $ (getReg reg a) == (getReg reg b)

filterOpcodes :: [[Opcode]] -> [Opcode]
filterOpcodes ops
  | all ((== 1) . length) ops = map head ops
  | otherwise = (filterOpcodes . map fn) ops
  where
  fn :: [Opcode] -> [Opcode]
  fn x
    | length x == 1 = x
    | otherwise = x \\ determined
  determined :: [Opcode]
  determined = (map head . filter ((== 1) . length)) ops

threeOrMoreOpcodes :: [Sample] -> [Sample]
threeOrMoreOpcodes = filter threeOrMore
  where
  threeOrMore :: Sample -> Bool
  threeOrMore (Sample x op y) = (length . filter (== y) $ allOps <*> pure x <*> pure op) >= 3
  allOps :: [Register -> Op -> Register]
  allOps = pure process <*> allOpcodes

toSamples :: String -> [Sample]
toSamples input = [Sample (toRegister x) (toOp y) (toRegister z)
                  | sample <- splitOn "\n\n" input
                  , let [x, y, z] = lines sample
                  ]

toTest :: String -> [Op]
toTest = map toOp . lines

examineSamples :: [Sample] -> [[Opcode]] -> [Register -> Op -> Register]
examineSamples [] ops = (map process . filterOpcodes) ops
examineSamples ((Sample before op after):xs) ops = examineSamples xs nextOps
  where
  index :: Int
  index = fst op
  current :: [Opcode]
  current = ops !! index
  filtered :: [Opcode]
  filtered = filter ((after ==) . flip (flip process before) op) current
  nextOps :: [[Opcode]]
  nextOps = (take index ops) ++ (filtered:(drop (index + 1) ops))

calculate :: [Register -> Op -> Register] -> [Op] -> Register -> Register
calculate _ [] reg = reg
calculate ops (op:xs) reg = calculate ops xs ((ops !! (fst op)) reg op)

findValue :: [Sample] -> [Op] -> Int
findValue samples test = value
  where
  (value, _, _, _) = calculate (examineSamples samples (replicate 16 allOpcodes)) test (0, 0, 0, 0)

main :: IO ()
main = do
  [samplesInput, testInput] <- readInput "input"
  let samples = toSamples samplesInput
  let test = toTest testInput
  print $ length (threeOrMoreOpcodes samples)
  print $ findValue samples test
