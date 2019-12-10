module Main where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import Data.Vector ((//))

import IntcodeComputer (runProgram, readData, Memory)

main :: IO ()
main = do 
  input <- T.readFile "Day2/input.txt"  
  let (Right numbers) = traverse T.decimal . T.split (== ',') $ input
  let program = V.fromList (fst <$> numbers)
  let result = head [100 * noun + verb | noun <- [0..99], verb <- [0..99], runProgram' program noun verb == 19690720]
  print result

runProgram' :: Memory -> Integer -> Integer -> Integer
runProgram' program noun verb = 
  let memory = program // [(1, noun), (2, verb)]
      (result, _, _, _) = runProgram memory [] 
  in  readData result 0