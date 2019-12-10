{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.List (intersect, elemIndex)
import Data.Maybe (maybeToList)

main :: IO ()
main = do 
  input <- T.readFile "Day6/input.txt"  
  let segments = T.lines input
  case traverse parseOrbit segments of 
    (Right numbers) -> 
      let directOrbits = M.fromList numbers 
          directAndIndirectOrbits = M.mapWithKey (\inOrbit _ -> orbits inOrbit directOrbits) directOrbits
          result = do 
            start <- maybeToList $ M.lookup "YOU" directOrbits
            target <- maybeToList $ M.lookup "SAN" directOrbits
            let startPaths = filter (elem start) $ M.elems directAndIndirectOrbits
            let targetPaths = filter (elem target) $ M.elems directAndIndirectOrbits
            startPath <- startPaths
            startIndex <- maybeToList $ elemIndex start startPath
            targetPath <- targetPaths
            targetIndex <- maybeToList $ elemIndex target targetPath
            intersection <- startPath `intersect` targetPath
            intersectionIndexInStartPath <- maybeToList $ elemIndex intersection startPath
            intersectionIndexInTargetPath <- maybeToList $ elemIndex intersection targetPath
            let startIntersectionDist = abs $ startIndex - intersectionIndexInStartPath
            let targetIntersectionDist = abs $ targetIndex - intersectionIndexInTargetPath
            return $ startIntersectionDist + targetIntersectionDist 
      in print $ minimum result
    (Left err) -> print err

parseOrbit :: Text -> Either Text (Text, Text)
parseOrbit line = case T.splitOn ")" line of
    [object, inOrbit] -> Right (inOrbit, object)
    _ -> Left $ "Unable to parse orbit from '" <> line <> "'"

orbits :: Text -> Map Text Text -> [Text]
orbits inOrbit directOrbits = case M.lookup inOrbit directOrbits of 
  (Just object) -> object : orbits object directOrbits
  _ -> []