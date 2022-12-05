
module Solutions.Solution01 where

import Data.List (groupBy, sort)
import Data.Function (on)
import Lib

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

solution = SolutionFN $ \input -> let parsed = parseInput input
                                  in (show $ part1 parsed , show $ part2 parsed)

-- $> runSolutionFN S1.solution <$> readInputDay 1
