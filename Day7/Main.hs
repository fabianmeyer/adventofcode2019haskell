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
    let perms = permutations [0..4]
    in maximum $ head . foldl runProgram' [0] <$> perms
  where runProgram' :: [Integer] -> Integer -> [Integer]
        runProgram' input phaseSetting  = 
            let (_, _, output, _) = runProgram program $ phaseSetting : input
            in output