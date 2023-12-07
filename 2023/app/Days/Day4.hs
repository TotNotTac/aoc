module Days.Day4 (part1, part2) where

import Data.List (group, sort, tails)
import Data.List.Split (splitOn)
import Lib (loadInput)

getInput = lines <$> loadInput 4

parseLine :: String -> ([Int], [Int])
parseLine input = (winning, ours)
 where
  [winning, ours] = map (map read . words) . splitOn "|" . drop 2 $ dropWhile (':' /=) input

takeWinningNumbers :: (Eq a) => [a] -> [a] -> [a]
takeWinningNumbers winning = filter (`elem` winning)

calculateScore :: [Int] -> Int
calculateScore [] = 0
calculateScore xs =
  (\x -> 2 ^ (x - 1))
    . length
    . map length
    . group
    . sort
    $ xs

solve1 =
  sum
    . map calculateScore
    . filter (not . null)
    . map (uncurry takeWinningNumbers . parseLine)

part1 = solve1 <$> getInput

-- Part 2

solve2 :: [([Int], [Int])] -> Int
solve2 xs = sum $ map solve2' $ tails xs

solve2' :: [([Int], [Int])] -> Int
solve2' [] = 0
solve2' (line : rest) =
  let currentScore = length $ uncurry takeWinningNumbers line
      newScratchCards = take currentScore $ tails rest
   in 1 + sum (map solve2' newScratchCards)

part2 = solve2 . map parseLine <$> getInput