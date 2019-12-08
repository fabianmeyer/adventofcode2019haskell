{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace
import Data.List (sort, sortOn, intersect, elemIndex)

data Vec = Vec Integer Integer deriving (Show, Eq)
deriving instance Ord Vec

instance Num Vec where
  (+) (Vec ax ay) (Vec bx by) = Vec (ax + bx) $ ay + by


main :: IO ()
main = do 
  input <- T.readFile "Day3_input.txt"  
  let [path1, path2] = positions (Vec 0 0) <$> fmap parseVector . T.split (== ',') <$> T.lines input
  let intersections = filter ((/= Vec 0 0)) . S.toList $ S.intersection (S.fromList path1) (S.fromList path2)
  let sortedManhattan = sort $ manhattanDist <$> intersections
  let sortedStep = sort $ stepDist path1 path2 <$> intersections
  print $ "Manhattan: " ++ (show . head $ sortedManhattan)
  print $ "Step: " ++ (show . head $ sortedStep)

parseVector :: Text -> Vec
parseVector direction = 
  let dir = T.head direction
      (Right (val, _)) = T.decimal $ T.tail direction
  in f dir val
  where f :: Char -> Integer -> Vec
        f 'R' v = Vec v 0
        f 'L' v = Vec (-v) 0
        f 'U' v = Vec 0 v
        f 'D' v = Vec 0 (-v)

manhattanDist :: Vec -> Integer
manhattanDist (Vec x y) = abs x + abs y

stepDist :: [Vec] -> [Vec] -> Vec -> Int
stepDist a b v = 
  let (Just ai) = elemIndex v a
      (Just bi) = elemIndex v b
  in ai + bi

positions :: Vec -> [Vec] -> [Vec]
positions start [] = [start]
positions start (v : rest) = (walk start v) ++ positions (start + v) rest

walk :: Vec -> Vec -> [Vec]
walk (Vec sx sy) (Vec vx 0) | vx >= 0 = [Vec (sx + x) sy | x <- [0 .. vx - 1]]
walk (Vec sx sy) (Vec vx 0) | vx < 0  = [Vec (sx - x) sy | x <- [0 .. (abs vx) - 1]]
walk (Vec sx sy) (Vec 0 vy) | vy >= 0 = [Vec sx (sy + y) | y <- [0 .. vy - 1]]
walk (Vec sx sy) (Vec 0 vy) | vy < 0  = [Vec sx (sy - y) | y <- [0 .. (abs vy) - 1]]