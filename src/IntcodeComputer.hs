module IntcodeComputer where 

import Data.Vector (Vector, (!), (//))
import Debug.Trace

type Address = Integer 
type Memory = Vector Integer

data Parameter = 
    PAddr PAddress 
  | PValue !Integer deriving Show

data PAddress = 
    PAbsAddr !Address
  | PRelAddr !Address deriving Show

data Instruction = 
    Add Parameter Parameter PAddress 
  | Mul Parameter Parameter PAddress 
  | Input PAddress
  | Output Parameter
  | JumpIfTrue Parameter Parameter
  | JumpIfFalse Parameter Parameter
  | LessThan Parameter Parameter PAddress
  | Equals Parameter Parameter PAddress
  | AdjustRelBase Parameter
  | Stop
  | Error
  deriving (Show)

data Computer = Computer {
  _memory :: !Memory
, _input :: ![Integer]
, _output :: ![Integer]
, _address :: !Address
, _relativeBase :: !Address
} deriving (Show)

runProgram :: Memory -> [Integer] -> Computer
runProgram program input = step $ Computer program input [] 0 0

step :: Computer -> Computer
step comp@(Computer memory _ _ address relBase) =
  let instruction = readInstruction memory relBase address
  in case instruction of
    Stop -> comp
    Error -> comp
    _ -> step $ runInstruction comp instruction
  
runInstruction :: Computer -> Instruction -> Computer
runInstruction (Computer memory input output address relativeBase) (Add pleft pright dest) = 
  let left = loadParam memory relativeBase pleft
      right = loadParam memory relativeBase pright
      result = left + right 
      memory' = storeData memory relativeBase dest result
      address' = address + 4
  in {-- trace (show left ++ " + " ++ show right ++ " = " ++ show result ++ " -> [" ++ show dest ++ "] = " ++ show result)--} Computer memory' input output address' relativeBase

runInstruction (Computer memory input output address relativeBase) (Mul pleft pright dest) = 
  let left = loadParam memory relativeBase pleft 
      right = loadParam memory relativeBase pright
      result = left * right 
      memory' = storeData memory relativeBase dest result
      address' = address + 4
  in {-- trace (show left ++ " * " ++ show right ++ " = " ++ show result ++ " -> [" ++ show dest ++ "] = " ++ show result)--} Computer memory' input output address' relativeBase

runInstruction (Computer memory input output address relativeBase) (Input dest) =  
  let (val : input') = input
      memory' = storeData memory relativeBase dest val 
      address' = address + 2
  in Computer memory' input' output address' relativeBase

runInstruction (Computer memory input output address relativeBase) (Output p) = 
  let output' = output ++ [loadParam memory relativeBase p]
      address' = address + 2
  in Computer memory input output' address' relativeBase

runInstruction (Computer memory input output address relativeBase) (JumpIfTrue pcondition dest) = 
  case loadParam memory relativeBase pcondition of 
    0 -> Computer memory input output (address + 3) relativeBase
    _ -> Computer memory input output (loadParam memory relativeBase dest) relativeBase

runInstruction (Computer memory input output address relativeBase) (JumpIfFalse pcondition dest) = 
  case loadParam memory relativeBase pcondition of 
    0 -> Computer memory input output (loadParam memory relativeBase dest) relativeBase
    _ -> Computer memory input output (address + 3) relativeBase

runInstruction (Computer memory input output address relativeBase) (LessThan pleft pright dest) = 
  let left = loadParam memory relativeBase pleft
      right = loadParam memory relativeBase pright
      result = if left < right then 1 else 0
      memory' = storeData memory relativeBase dest result
      address' = address + 4
  in Computer memory' input output address' relativeBase

runInstruction (Computer memory input output address relativeBase) (Equals pleft pright dest) = 
  let left = loadParam memory relativeBase pleft
      right = loadParam memory relativeBase pright
      result = if left == right then 1 else 0
      memory' = storeData memory relativeBase dest result
      address' = address + 4 
  in Computer memory' input output address' relativeBase
  
runInstruction (Computer memory input output address relativeBase) (AdjustRelBase pOffset) = 
    let offset = loadParam memory relativeBase pOffset
        relativeBase' = relativeBase + offset
        address' = address + 2
    in Computer memory input output address' relativeBase'

runInstruction (Computer memory input output address relativeBase) _ = Computer memory input output (address + 1) relativeBase
    
loadParam :: Memory -> Integer -> Parameter -> Integer
loadParam memory relBase (PAddr addr) = readData memory relBase addr 
loadParam _ _ (PValue val) = val

resolveAddr :: Integer -> PAddress -> Integer
resolveAddr _ (PAbsAddr absAddr) = absAddr
resolveAddr relBase (PRelAddr relAddr) = relAddr + relBase

readData :: Memory -> Address -> PAddress -> Integer
readData memory relBase address = 
  let data' = memory ! fromIntegral (resolveAddr relBase address)
  in {--trace ("Read " ++ show data' ++ " from " ++ show address)--} data'

storeData :: Memory -> Address -> PAddress -> Integer -> Memory
storeData memory relBase address val = 
  {--trace ("Wrote " ++ show val ++ " to " ++ show address) $--} memory // [(fromIntegral (resolveAddr relBase address), val)]

readInstruction :: Memory -> Address -> Address -> Instruction
readInstruction memory relativeBase address =  
    let instr = readData memory relativeBase $ PAbsAddr address 
        (paramModesCode, opcode) = instr `divMod` 100
        paramModes = snd <$> iterate (\(rest, _) -> rest `divMod` 10) (paramModesCode `divMod` 10)
        paramVals = (\offset -> readData memory relativeBase $ PAbsAddr $ address + offset) <$> [1..]
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
      readInstruction' 9 (p0 : _) =  AdjustRelBase p0
      readInstruction' 99 _ = Stop
      readInstruction' _ _ = Error

param :: Integer -> Integer -> Parameter
param 0 address = PAddr $ PAbsAddr address
param 1 value = PValue value
param _ value = PAddr $ PRelAddr value
    