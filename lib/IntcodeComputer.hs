module IntcodeComputer where

import Data.Vector (Vector, (!), (//))
import Debug.Trace

type Address = Integer 
type Memory = Vector Integer

data Parameter = PAddr Address | PValue Integer deriving Show

data Instruction = 
    Add Parameter Parameter Address 
  | Mul Parameter Parameter Address 
  | Input Address
  | Output Parameter
  | JumpIfTrue Parameter Parameter
  | JumpIfFalse Parameter Parameter
  | LessThan Parameter Parameter Address
  | Equals Parameter Parameter Address
  | Stop
  | Error
  deriving (Show)

runProgram :: Memory -> [Integer] -> (Memory, [Integer], [Integer], Address)
runProgram program input  = 
  let result = until finished step (program, input, [], 0) 
  in result

step :: (Memory, [Integer], [Integer], Address) -> (Memory, [Integer], [Integer], Address)
step (memory, input, output, address) =
  let instruction = readInstruction memory address
  in runInstruction {--$ trace (show instruction)--} instruction
  where 
    runInstruction :: Instruction -> (Memory, [Integer], [Integer], Address)
    runInstruction (Add pleft pright dest) = 
      let left = loadParam memory pleft
          right = loadParam memory pright
          result = left + right
          memory' = storeData memory dest result
      in (memory', input, output, address + 4)
    runInstruction (Mul pleft pright dest) = 
      let left = loadParam memory pleft
          right = loadParam memory pright
          result = left * right
          memory' = storeData memory dest result
      in (memory', input, output, address + 4)
    runInstruction (Input dest) = 
      let (val : input') = input
          memory' = storeData memory dest val
      in (memory', input', output, address + 2)
    runInstruction (Output p) = 
      let output' = output ++ [loadParam memory p]
      in (memory, input, output', address + 2)
    runInstruction (JumpIfTrue pcondition dest) = 
      case loadParam memory pcondition of 
        0 -> (memory, input, output, address + 3)
        _ -> (memory, input, output, loadParam memory dest)
    runInstruction (JumpIfFalse pcondition dest) = 
      case loadParam memory pcondition of 
        0 -> (memory, input, output, loadParam memory dest)
        _ -> (memory, input, output, address + 3)
    runInstruction (LessThan pleft pright dest) = 
      let left = loadParam memory pleft
          right = loadParam memory pright
          result = if left < right then 1 else 0
          memory' = storeData memory dest result
      in (memory', input, output, address + 4)
    runInstruction (Equals pleft pright dest) = 
      let left = loadParam memory pleft
          right = loadParam memory pright
          result = if left == right then 1 else 0
          memory' = storeData memory dest result
      in (memory', input, output, address + 4)
    runInstruction _ = (memory, input, output, address + 1)
    


loadParam :: Memory -> Parameter -> Integer
loadParam _ (PValue val) = val
loadParam memory (PAddr addr) = readData memory addr

finished :: (Memory, [Integer], [Integer], Address) -> Bool
finished (memory, _, _, addr) = case readInstruction memory addr of 
  Stop -> True
  Error -> True
  _ -> False

readData :: Memory -> Address -> Integer
readData memory address = 
  let data' = memory ! fromIntegral address
  in {--trace ("Read " ++ show data' ++ " from " ++ show address)--} data'

storeData :: Memory -> Address -> Integer -> Memory
storeData memory address val = 
  {--trace ("Wrote " ++ show val ++ " to " ++ show address) $--} memory // [(fromIntegral address, val)]

readInstruction :: Memory -> Address -> Instruction
readInstruction memory address = 
    let instr = readData memory address
        (paramModesCode, opcode) = instr `divMod` 100
        paramModes = snd <$> iterate (\(rest, _) -> rest `divMod` 10) (paramModesCode `divMod` 10)
        paramVals = (\offset -> readData memory $ address + offset) <$> [1..]
        params = zipWith param paramModes paramVals
    in  readInstruction' opcode params
    where 
      readInstruction' :: Integer -> [Parameter] -> Instruction
      readInstruction' 1 (p0 : p1 : PAddr p2 : _) = Add p0 p1 p2 
      readInstruction' 2 (p0 : p1 : PAddr p2 : _)  = Mul p0 p1 p2 
      readInstruction' 3 (PAddr p0 : _)   = Input p0
      readInstruction' 4 (p0 : _) = Output p0
      readInstruction' 5 (p0 : p1 : _) = JumpIfTrue p0 p1
      readInstruction' 6 (p0 : p1 : _) = JumpIfFalse p0 p1
      readInstruction' 7 (p0 : p1 : PAddr p2 : _) = LessThan p0 p1 p2
      readInstruction' 8 (p0 : p1 : PAddr p2 : _) = Equals p0 p1 p2
      readInstruction' 99 _ = Stop
      readInstruction' _ _ = Error

param :: Integer -> Integer -> Parameter
param 0 address = PAddr address
param _ value = PValue value
    

    

    