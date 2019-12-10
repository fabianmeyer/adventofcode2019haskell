module Main where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V

import IntcodeComputer (runProgram)
  
main :: IO ()
main = do 
  input <- T.readFile "Day5/input.txt"  
  let segments = T.split (== ',') input
  case traverse (T.signed T.decimal) segments of 
    (Right numbers) ->
      let program = V.fromList $ fst <$> numbers
          result = runProgram program [5] 
      in print result
    (Left err) -> print err