module Main where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import Data.List (permutations)

import IntcodeComputer (Memory, runProgram) 
  
main :: IO ()
main = do 
  input <- T.readFile "Day7/input.txt"  
  let segments = T.split (== ',') input
  case traverse (T.signed T.decimal) segments of 
    (Right numbers) ->
      let program = V.fromList $ fst <$> numbers
          result = exec program
      in print result
    (Left err) -> print err

exec :: Memory -> Integer
exec program = 
    let perms = permutations [5..9]
    in maximum $ exec' program <$> perms

exec' :: Memory -> [Integer] -> Integer
exec' program [phaseA, phaseB, phaseC, phaseD, phaseE] = 
  let outA = runProgram' $ phaseA : 0 : outE
      outB = runProgram' $ phaseB : outA
      outC = runProgram' $ phaseC : outB
      outD = runProgram' $ phaseD : outC
      outE = runProgram' $ phaseE : outD
  in last outE
  where runProgram' :: [Integer] -> [Integer]
        runProgram' input  = 
            let (_, _, output, _) = runProgram program input
            in output
exec' _ _ = error "Waaaaaah, not enough phases!"