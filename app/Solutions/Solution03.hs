-- | 

module Solutions.Solution03 where

{-
One Elf has the important job of loading all of the rucksacks with supplies for
the jungle journey. Unfortunately, that Elf didn't quite follow the packing
instructions, and so a few items now need to be rearranged.

Each rucksack has two large compartments. All items of a given type are meant to
go into exactly one of the two compartments. The Elf that did the packing failed
to follow this rule for exactly one item type per rucksack.

The Elves have made a list of all of the items currently in each rucksack (your
puzzle input), but they need your help finding the errors. Every item type is
identified by a single lowercase or uppercase letter (that is, a and A refer to
different types of items).
-}

import Lib

import Data.List.Split (chunksOf)
import Data.List.Extra (allSame)
import Control.Monad (guard)
import Data.Char


parseInput = lines

findCommonChar :: (String, String) -> Char
findCommonChar (xs, ys) = head [ x | x <- xs, x `elem` ys ]

calcScore :: Char -> Int
calcScore x = ord x - (if x > 'Z' then 96 else 38)

part1
  = sum
  . map calcScore
  . map (\xs -> findCommonChar $ splitAt (length xs `div` 2) xs)

findBadge :: [[Char]] -> Char
findBadge [xs, ys, zs] = head [ x | x <- xs, y <- ys, z <- zs, allSame [x,y,z] ]

part2
  = sum
  . map (calcScore . findBadge)
  . chunksOf 3

solution = SolutionFN $ \input -> let parsed = parseInput input
                                  in (show $ part1 parsed , show $ part2 parsed)

-- $> runSolutionFN S3.solution <$> readInputDay 3
