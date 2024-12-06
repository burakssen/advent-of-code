module Main where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.HashMap.Strict qualified as HashMap
import System.Environment (getArgs)

data InstructionType
  = ASSIGN String
  | AND String String
  | OR String String
  | LSHIFT String String
  | RSHIFT String String
  | NOT String
  deriving (Show)

data Instruction = Instruction
  { instructionType :: InstructionType,
    result :: String
  }
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> processFile readFile filename
    _ -> putStrLn "Usage: cabal run . -- <input.txt>"

processFile :: (FilePath -> IO String) -> FilePath -> IO ()
processFile reader filename = do
  contents <- reader filename
  let ls = lines contents
  let instructions = parseInstructions ls
  let (result, _) = eval "a" instructions HashMap.empty
  let (result2, _) = eval "a" (Instruction (ASSIGN (show result)) "b" : instructions) HashMap.empty
  putStrLn $ "Part 1: " ++ show result
  putStrLn $ "Part 2: " ++ show result2

parseInstructions :: [String] -> [Instruction]
parseInstructions = Prelude.map parseInstruction

parseInstruction :: String -> Instruction
parseInstruction s = case words s of
  [a, "->", b] -> Instruction (ASSIGN a) b
  [a, "AND", b, "->", c] -> Instruction (AND a b) c
  [a, "OR", b, "->", c] -> Instruction (OR a b) c
  [a, "LSHIFT", b, "->", c] -> Instruction (LSHIFT a b) c
  [a, "RSHIFT", b, "->", c] -> Instruction (RSHIFT a b) c
  ["NOT", a, "->", b] -> Instruction (NOT a) b
  _ -> error "Invalid instruction"

getInstruction :: String -> [Instruction] -> Instruction
getInstruction s = head . Prelude.filter (\i -> result i == s)

eval :: String -> [Instruction] -> HashMap.HashMap String Int -> (Int, HashMap.HashMap String Int)
eval wire instrs hm =
  case HashMap.lookup wire hm of
    Just value -> (value, hm)
    Nothing ->
      let instruction = getInstruction wire instrs
          (value, newHm) = evalInstruction instruction instrs hm
          updatedHm = HashMap.insert wire value newHm
       in (value, updatedHm)

evalInstruction :: Instruction -> [Instruction] -> HashMap.HashMap String Int -> (Int, HashMap.HashMap String Int)
evalInstruction (Instruction (ASSIGN a) _) instrs hm = evalExpr a instrs hm
evalInstruction (Instruction (AND a b) _) instrs hm =
  let (valueA, newHm) = evalExpr a instrs hm
      (valueB, newHm2) = evalExpr b instrs newHm
   in (valueA .&. valueB, newHm2)
evalInstruction (Instruction (OR a b) _) instrs hm =
  let (valueA, newHm) = evalExpr a instrs hm
      (valueB, newHm2) = evalExpr b instrs newHm
   in (valueA .|. valueB, newHm2)
evalInstruction (Instruction (LSHIFT a b) _) instrs hm =
  let (valueA, newHm) = evalExpr a instrs hm
      (valueB, newHm2) = evalExpr b instrs newHm
   in (valueA `shiftL` valueB, newHm2)
evalInstruction (Instruction (RSHIFT a b) _) instrs hm =
  let (valueA, newHm) = evalExpr a instrs hm
      (valueB, newHm2) = evalExpr b instrs newHm
   in (valueA `shiftR` valueB, newHm2)
evalInstruction (Instruction (NOT a) _) instrs hm =
  let (valueA, newHm) = evalExpr a instrs hm
   in (complement valueA, newHm)

evalExpr :: String -> [Instruction] -> HashMap.HashMap String Int -> (Int, HashMap.HashMap String Int)
evalExpr s instrs hm = case reads s of
  [(n, "")] -> (n, hm)
  _ ->
    let (result, newHm) = eval s instrs hm
     in (result, newHm)
