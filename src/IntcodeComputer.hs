module IntcodeComputer where 

  import Data.Vector (Vector, (!), (//))
  
  type Address = Integer 
  type Memory = Vector Integer
  
  data Parameter = 
      PAddr Address 
    | PRelAddr Address
    | PValue Integer deriving Show
  
  data Instruction = 
      Add Parameter Parameter Address 
    | Mul Parameter Parameter Address 
    | Input Address
    | Output Parameter
    | JumpIfTrue Parameter Parameter
    | JumpIfFalse Parameter Parameter
    | LessThan Parameter Parameter Address
    | Equals Parameter Parameter Address
    | AdjustRelBase Parameter
    | Stop
    | Error
    deriving (Show)
  
  data Computer = Computer {
    _memory :: Memory
  , _input :: [Integer]
  , _output :: [Integer]
  , _address :: Address
  , _relativeBase :: Address
  }
  
  runProgram :: Memory -> [Integer] -> Computer
  runProgram program input  = until finished step $ Computer program input [] 0 0
  
  step :: Computer -> Computer
  step (Computer memory input output address relativeBase) =
    let instruction = readInstruction memory address
    in runInstruction {--$ trace (show instruction)--} instruction
    where 
      runInstruction :: Instruction -> Computer
      runInstruction (Add pleft pright dest) = 
        let left = loadParam memory relativeBase pleft
            right = loadParam memory relativeBase pright
            result = left + right
            memory' = storeData memory dest result
            address' = address + 4
        in Computer memory' input output address' relativeBase
      runInstruction (Mul pleft pright dest) = 
        let left = loadParam memory relativeBase pleft 
            right = loadParam memory relativeBase pright
            result = left * right
            memory' = storeData memory dest result
            address' = address + 4
        in Computer memory' input output address' relativeBase
      runInstruction (Input dest) =  
        let (val : input') = input
            memory' = storeData memory dest val
            address' = address + 2
        in Computer memory' input' output address' relativeBase
      runInstruction (Output p) = 
        let output' = output ++ [loadParam memory relativeBase p]
            address' = address + 2
        in Computer memory input output' address' relativeBase
      runInstruction (JumpIfTrue pcondition dest) = 
        case loadParam memory relativeBase pcondition of 
          0 -> Computer memory input output (address + 3) relativeBase
          _ -> Computer memory input output (loadParam memory relativeBase dest) relativeBase
      runInstruction (JumpIfFalse pcondition dest) = 
        case loadParam memory relativeBase pcondition of 
          0 -> Computer memory input output (loadParam memory relativeBase dest) relativeBase
          _ -> Computer memory input output (address + 3) relativeBase
      runInstruction (LessThan pleft pright dest) = 
        let left = loadParam memory relativeBase pleft
            right = loadParam memory relativeBase pright
            result = if left < right then 1 else 0
            memory' = storeData memory dest result
            address' = address + 4
        in Computer memory' input output address' relativeBase
      runInstruction (Equals pleft pright dest) = 
        let left = loadParam memory relativeBase pleft
            right = loadParam memory relativeBase pright
            result = if left == right then 1 else 0
            memory' = storeData memory dest result
            address' = address + 4 
        in Computer memory' input output address' relativeBase
      runInstruction (AdjustRelBase pOffset) = 
          let offset = loadParam memory relativeBase pOffset
              relativeBase' = relativeBase + offset
              address' = address + 2
          in Computer memory input output address' relativeBase'
      runInstruction _ = Computer memory input output (address + 1) relativeBase
      
  
  
  loadParam :: Memory -> Integer -> Parameter -> Integer
  loadParam memory _ (PAddr addr) = readData memory addr
  loadParam memory relBase (PRelAddr relAddr) = readData memory (relAddr + relBase)
  loadParam _ _ (PValue val) = val
  
  finished :: Computer -> Bool
  finished (Computer memory _ _ addr _) = case readInstruction memory addr of 
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
        readInstruction' 9 (p0 : _) =  AdjustRelBase p0
        readInstruction' 99 _ = Stop
        readInstruction' _ _ = Error
  
  param :: Integer -> Integer -> Parameter
  param 0 address = PAddr address
  param 1 value = PValue value
  param _ value = PRelAddr value
      