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

opcodes :: [Opcode]
opcodes = [Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr]

ops :: [Register -> Op -> Register]
ops = pure process <*> opcodes

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
  threeOrMore (Sample before op after) = (length . filter (== after) $ ops <*> pure before <*> pure op) >= 3

toSamples :: String -> [Sample]
toSamples input = [Sample (toRegister x) (toOp y) (toRegister z)
                  | sample <- splitOn "\n\n" (head (splitOn "\n\n\n\n" input))
                  , let [x, y, z] = lines sample]

main :: IO ()
main = do
  input <- readFile "input"
  let samples = toSamples input
  print $ length (threeOrMoreOpcodes samples)
