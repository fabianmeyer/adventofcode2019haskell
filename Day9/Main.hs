module Main where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V

import IntcodeComputer (runProgram, _output) 
  
main :: IO ()
main = do  
  input <- T.readFile "Day9/input.txt"  
  let segments = T.split (== ',') input
  case traverse (T.signed T.decimal) segments of 
    (Right numbers) ->
      let program = V.fromList . take 1024 $ (fst <$> numbers) ++ repeat 0
          result = _output $ runProgram program [1]
      in print result
    (Left err) -> print err