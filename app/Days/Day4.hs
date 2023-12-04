{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Days.Day4 (part1, part2) where

import Data.List (group, sort)
import Data.List.Split (splitOn)
import Lib (loadInput)

getInput = lines <$> loadInput 4

parseLine :: String -> ([Int], [Int])
parseLine input = (winning, ours)
 where
  [winning, ours] = map (map read . words) . splitOn "|" . drop 2 $ dropWhile (':' /=) input

takeWinningNumbers :: (Eq a) => [a] -> [a] -> [a]
takeWinningNumbers winning = filter (`elem` winning)

calculateScore :: [Int] -> Integer
calculateScore =
  (\x -> 2 ^ (x - 1))
    . length
    . map length
    . group
    . sort

solve1 =
  sum
    . map calculateScore
    . filter (not . null)
    . map (uncurry takeWinningNumbers . parseLine)

part1 = solve1 <$> getInput
part2 = undefined