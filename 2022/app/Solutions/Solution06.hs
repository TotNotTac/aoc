
module Solutions.Solution06 where

import Control.Monad
import Data.Maybe (fromJust)
import Data.List (tails, find)
import Data.List.Extra (anySame)
import Lib

noneSameN :: Int -> [Char] -> Bool
noneSameN n xs | length xs >= n = not (anySame $ take n xs)
               | otherwise      = False

findFirstNUniqueChars n
  = (+n)
  . fst . fromJust
  . find (noneSameN n . snd)
  . enumerate
  . tails

part1 = findFirstNUniqueChars 4
part2 = findFirstNUniqueChars 14

solution = SolutionFN $ (show . part1) &&& (show . part2)

-- $> S6.part1 <$> readInputDay 6

-- $> S6.part2 <$> readInputDay 6
