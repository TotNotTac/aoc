-- |

module Solutions.Solution04 where

{-
Space needs to be cleared before the last supplies can be unloaded from the
ships, and so several Elves have been assigned the job of cleaning up sections
of the camp. Every section has a unique ID number, and each Elf is assigned a
range of section IDs.

However, as some of the Elves compare their section assignments with each other,
they've noticed that many of the assignments overlap. To try to quickly find
overlaps and reduce duplicated effort, the Elves pair up and make a big list of
the section assignments for each pair (your puzzle input).
-}

import Lib (readInputDay, SolutionFN(..))

type Range = (Int, Int)
type Pair = (Range, Range)

parseRange :: String -> Range
parseRange xs = (read lb, read $ tail ub)
  where (lb, ub) = span (/= '-') xs

parsePair :: String -> Pair
parsePair xs = (parseRange lb, parseRange $ tail ub)
  where (lb, ub) = span (/= ',') xs

parseInput
 = map parsePair
 . lines

fullyContainsOther :: Pair -> Bool
fullyContainsOther ((lb1, rb1), (lb2, rb2))
  = leftContainsRight || rightContainsLeft
  where leftContainsRight = lb1 <= lb2 && rb1 >= rb2
        rightContainsLeft = lb2 <= lb1 && rb2 >= rb1

overlaps :: Pair -> Bool
overlaps ((lb1, rb1), (lb2, rb2))
  = not $ null [ () | x <- [lb1..rb1], y <- [lb2..rb2], x==y]

part1 = length . filter fullyContainsOther
part2 = length . filter overlaps

solution = SolutionFN $ \input -> let parsed = parseInput input
                                  in (show $ part1 parsed , show $ part2 parsed)

-- $> S4.part1 . S4.parseInput <$> readInputDay 4

-- $> S4.part2 . S4.parseInput <$> readInputDay 4
