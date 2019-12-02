module Main where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Read as T

main :: IO ()
main = do 
  input <- T.readFile "Day1_input.txt"  
  let inputLines = T.lines input
  let (Right numbers) = traverse T.decimal inputLines
  let result = sum $ requiredFuel . fst <$> numbers
  
  T.putStrLn . T.pack . show $ result
  
requiredFuel :: Integer -> Integer
requiredFuel 0 = 0
requiredFuel mass = 
  let result = max 0 . flip (-) 2 . floor . (/ 3) . fromIntegral $ mass
  in result + requiredFuel result
