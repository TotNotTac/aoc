
module Solutions.Solution01 where

import Data.List (groupBy, sort)
import Data.Function (on)
import Lib

{-
The jungle must be too overgrown and difficult to navigate in vehicles or access
from the air; the Elves' expedition traditionally goes on foot. As your boats
approach land, the Elves begin taking inventory of their supplies. One important
consideration is food - in particular, the number of Calories each Elf is
carrying (your puzzle input).

The Elves take turns writing down the number of Calories contained by the
various meals, snacks, rations, etc. that they've brought with them, one item
per line. Each Elf separates their own inventory from the previous Elf's
inventory (if any) by a blank line.
-}

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
