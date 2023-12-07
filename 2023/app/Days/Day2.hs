module Days.Day2 (part1, part2) where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Lib

getInput = lines <$> loadInput 2

part1 = do
  input <- getInput
  let result = sum $ map fst $ filter snd $ zip [1 ..] $ map solve1 input
  print result

gameIsPossible :: Map.Map String Int -> Bool
gameIsPossible game =
  and
    $ catMaybes
      [ (<= 12) <$> Map.lookup "red" game
      , (<= 13) <$> Map.lookup "green" game
      , (<= 14) <$> Map.lookup "blue" game
      ]

solve1 input = and setsPossible
 where
  setsPossible = map (gameIsPossible . solveSet) sets
  sets = splitOn "; " $ drop 2 $ dropWhile (/= ':') input

parseCubes :: String -> (String, Int)
parseCubes input = (color, read count)
 where
  [count, color] = words input

solveSet :: [Char] -> Map.Map String Int
solveSet = Map.fromListWith (+) . map parseCubes . splitOn ", "

emptyCubeSet = [("red", 0), ("green", 0), ("blue", 0)]

solve2 :: String -> Int
solve2 =
  product
    . Map.elems
    . Map.unionsWith max
    . map
      ( Map.fromListWith max
          . (emptyCubeSet ++)
          . map parseCubes
          . splitOn ", "
      )
    . splitOn "; "
    . drop 2
    . dropWhile (/= ':')

part2 = do
  input <- getInput
  print $ sum $ map solve2 input