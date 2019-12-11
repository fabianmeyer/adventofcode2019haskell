module Main where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Char (digitToInt)
  
width :: Int
width = 25

height :: Int
height = 6

chunks :: Int -> [Int] -> [[Int]]
chunks _ [] = []
chunks n l = take n l : chunks n (drop n l)

main :: IO ()
main = do 
  input <- T.readFile "Day8/input.txt"  
  let segments =  digitToInt <$> T.unpack input
  let layers = chunks (width * height) segments
  let image = render layers <$> [0 .. width * height - 1]
  mapM_ print $ chunks width image

render :: [[Int]] -> Int  -> Int
render (layer : layers) pixel = 
  case layer !! pixel of 
    2 -> render layers pixel
    c -> c
render [] _ = 2
