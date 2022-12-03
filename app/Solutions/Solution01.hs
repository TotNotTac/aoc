
module Solutions.Solution01 where

import Data.List (groupBy, sort)
import Data.Function (on)

parseInput :: String -> [[Int]]
parseInput
  = (map . map) read
  . filter (/= [""])
  . groupBy ((&&) `on` (/= ""))
  . lines

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2
  = sum
  . take 3
  . reverse
  . sort
  . map sum

solution = ( show . part1 . parseInput <$> readFile "inputs/day1.txt"
           , show . part2 . parseInput <$> readFile "inputs/day1.txt"
           )

