{-# LANGUAGE StandaloneDeriving #-}

module Main where
import Data.List (find, zip4)
import Data.Maybe (isJust, isNothing)

data Vec = Vec Integer Integer deriving (Show, Eq)
deriving instance Ord Vec

instance Num Vec where
  (+) (Vec ax ay) (Vec bx by) = Vec (ax + bx) $ ay + by


main :: IO ()
main = do 
  let matches = filter matchesCriterias [245182 .. 790572]
  print $ length matches

matchesCriterias :: Integer -> Bool
matchesCriterias input = 
  let digs = digits input
  in twoAdjacentDigits digs && neverDecreases digs

twoAdjacentDigits :: [Int] -> Bool
twoAdjacentDigits input = 
  let input' = -1 : input ++ [-1]
  in isJust . find twoAdjacentDigits' $ zip4 input' (tail input') (tail . tail $ input') (tail . tail . tail $ input')
  where twoAdjacentDigits' (a, b, c, d) = a /= b && b == c && c /= d

neverDecreases :: [Int] -> Bool
neverDecreases input = isNothing . find (uncurry (>)) $ zip input $ tail input

digits :: Integer -> [Int]
digits = map (read . (:[])) . show