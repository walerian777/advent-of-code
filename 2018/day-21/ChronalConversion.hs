-- https://adventofcode.com/2018/day/21

module ChronalConversion where

import Data.Sequence (Seq, fromList, index, adjust', update)
import qualified Data.Sequence as Seq
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Bits ((.&.), (.|.))
import Data.Bool (bool)
import Debug.Trace

type Elements = (Int, Int, Int)
type Op = (Opcode, Elements)
type Register = Seq Int
type Program = (Int, Seq Op)

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
allOpcodes = [ Addr, Addi
             , Mulr, Muli
             , Banr, Bani
             , Borr, Bori
             , Setr, Seti
             , Gtir, Gtri
             , Gtrr, Eqir
             , Eqri, Eqrr
             ]

allOpnames :: [String]
allOpnames = [ "addr", "addi"
             , "mulr", "muli"
             , "banr", "bani"
             , "borr", "bori"
             , "setr", "seti"
             , "gtir", "gtri"
             , "gtrr", "eqir"
             , "eqri", "eqrr"
             ]

toInt :: String -> Int
toInt = read

toOp :: String -> Op
toOp str = (opcode, (a, b, c))
  where
  (op:regs) = splitOn " " str
  [a, b, c] = map toInt regs
  opcode :: Opcode
  opcode = allOpcodes !! fromJust (elemIndex op allOpnames)

toProgram :: [String] -> Program
toProgram (x:xs) = (ip, ops)
    where
    ip :: Int
    ip  = toInt (last x:[])
    ops :: Seq Op
    ops = fromList (map toOp xs)

getReg :: Register -> Int -> Int
getReg = index

setReg :: Register -> Int -> Int -> Register
setReg = flip (flip . update)

process :: Op -> Register -> Register
process (Addr, (a, b, c)) reg = setReg reg c $ (getReg reg a) + (getReg reg b)
process (Addi, (a, b, c)) reg = setReg reg c $ (getReg reg a) + b
process (Mulr, (a, b, c)) reg = setReg reg c $ (getReg reg a) * (getReg reg b)
process (Muli, (a, b, c)) reg = setReg reg c $ (getReg reg a) * b
process (Banr, (a, b, c)) reg = setReg reg c $ (getReg reg a) .&. (getReg reg b)
process (Bani, (a, b, c)) reg = setReg reg c $ (getReg reg a) .&. b
process (Borr, (a, b, c)) reg = setReg reg c $ (getReg reg a) .|. (getReg reg b)
process (Bori, (a, b, c)) reg = setReg reg c $ (getReg reg a) .|. b
process (Setr, (a, b, c)) reg = setReg reg c $ (getReg reg a)
process (Seti, (a, b, c)) reg = setReg reg c $ a
process (Gtir, (a, b, c)) reg = setReg reg c $ bool 0 1 $ a > (getReg reg b)
process (Gtri, (a, b, c)) reg = setReg reg c $ bool 0 1 $ (getReg reg a) > b
process (Gtrr, (a, b, c)) reg = setReg reg c $ bool 0 1 $ (getReg reg a) > (getReg reg b)
process (Eqir, (a, b, c)) reg = setReg reg c $ bool 0 1 $ a == (getReg reg b)
process (Eqri, (a, b, c)) reg = setReg reg c $ bool 0 1 $ (getReg reg a) == b
process (Eqrr, (a, b, c)) reg = setReg reg c $ bool 0 1 $ (getReg reg a) == (getReg reg b)

execute :: Program -> Register -> Register
execute (ip, ops) register
  | regIndex == 28 = register
  | length ops > regIndex = execute (ip, ops) nextRegister
  | otherwise = register
  where
  regIndex :: Int
  regIndex = index register ip
  nextRegister :: Register
  nextRegister
    | regIndex == 28 = adjust' (+ 1) ip (process (index ops regIndex) register)
    | otherwise = adjust' (+ 1) ip (process (index ops regIndex) register)

main :: IO ()
main = do
  input <- fmap lines (readFile "input")
  let program = toProgram input
  let register = fromList [0, 0, 0, 0, 0, 0]
  print $ execute program register
