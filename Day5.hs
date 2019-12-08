module Main where

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Debug.Trace

type Address = Integer
type Memory = Vector Integer

data Parameter = PAddr Address | PValue Integer deriving Show

data Instruction = 
    Add Parameter Parameter Parameter 
  | Mul Parameter Parameter Parameter 
  | Input Parameter
  | Output Parameter
  | Stop
  | Error
  deriving (Show)
  
main :: IO ()
main = do 
  input <- T.readFile "Day2_input.txt"  
  let (Right numbers) = traverse T.decimal . T.split (== ',') $ input
  let program = V.fromList (fst <$> numbers)
  let result = head [100 * noun + verb | noun <- [0..99], verb <- [0..99], runProgram program noun verb == 19690720]
  print result

runProgram :: Memory -> Integer -> Integer -> Integer
runProgram program noun verb = 
  let memory = program // [(1, noun), (2, verb)]
      firstInstruction = readInstruction memory 0
      (result, _, _) = until finished step (memory, 0, firstInstruction) 
  in  readData result 0

finished :: (Memory, Address, Instruction) -> Bool
finished (_, _, Stop) = True
finished (_, _, Error) = True
finished _ = False

step :: (Memory, Address, Instruction) -> (Memory, Address, Instruction)
step (memory, address, instruction) = 
  let result = runInstruction memory instruction
      nextAddress = address + 4
      nextInstruction = readInstruction result nextAddress
  in (result, nextAddress, nextInstruction)

readData :: Memory -> Address -> Integer
readData memory address = memory ! fromIntegral address

storeData :: Memory -> Address -> Integer -> Memory
storeData memory address val = memory // [(fromIntegral address, val)]

readInstruction :: Memory -> Address -> Instruction
readInstruction memory address = 
    let opcode = readData memory address
    in  readInstruction' opcode
    where 
      readInstruction' :: Integer -> Instruction
      readInstruction' 1 = Add (PAddr $readData memory $ address + 1) (PAddr $ readData memory $ address + 2) (PAddr $ readData memory $ address + 3)
      readInstruction' 2 = Mul (PAddr $readData memory $ address + 1) (PAddr $ readData memory $ address + 2) (PAddr $ readData memory $ address + 3)
      readInstruction' 3 = Input (PAddr $ readData memory $ address + 1)
      readInstruction' 4 = Output (PAddr $ readData memory $ address + 1)
      readInstruction' 99 = Stop
      readInstruction' _ = Error
  
runInstruction :: Memory -> Instruction -> Memory
runInstruction memory (Add (PAddr left) (PAddr right) (PAddr dest)) = storeData memory dest $ readData memory left + readData memory right
runInstruction memory (Mul (PAddr left) (PAddr right) (PAddr dest)) = storeData memory dest $ readData memory left * readData memory right
runInstruction memory _ = memory
      
incr :: Instruction -> Address
incr Add{} = 4
incr Mul{} = 4
incr Input{} = 2
incr Output{} = 2
incr Stop = 1
incr Error = 1

    

    