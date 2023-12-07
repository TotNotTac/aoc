module Solution6 where

import System.Directory

-- This solution made the most logical sense to me
-- It'll work fine with a lower amount of generations
-- but as soon as we go a little higher, it starts taking
-- ages to calculate the answer.

parse1 :: String -> [Integer]
parse1 = map read . split . takeWhile (/= '\n')

split [] = []
split (c:cs)
      | c == ','  = split cs
      | otherwise = [c] : split cs

ageBlowFish :: Integer -> [Integer]
ageBlowFish x | x == 0 = [6, 8]
ageBlowFish x          = [x-1]

tick1 = concat . map ageBlowFish

part1 = do
  input <- do
        currentDir <- getCurrentDirectory
        txt <- readFile $ currentDir ++ "/src/inputs/" ++ "6.txt"
        return $ parse1 txt
  let generations = iterate tick1 input

  print $ length $ generations !! 80

-- Let's try to make a more efficient solution for part 2

data Day -- A bit of a smarter datatype that holds the amount of fish grouped by the amount of days to reproduction
  = Day Integer -- 0 Days
        Integer -- 1 Day
        Integer -- 2 Days
        Integer -- 3 Days
        Integer -- 4 Days
        Integer -- 5 Days
        Integer -- 6 Days
        Integer -- 7 Days
        Integer -- 8 Days
    deriving (Show)

emptyDay = Day 0 0 0 0 0 0 0 0 0

tick2 :: Day -> Day
tick2 (Day d0 d1 d2 d3 d4 d5 d6 d7 d8)
  = (Day
      d1        -- day 0
      d2        -- day 1
      d3        -- day 2
      d4        -- day 3
      d5        -- day 4
      d6        -- day 5
      (d7 + d0) -- day 6
      d8        -- day 7
      d0        -- day 8
    )

putFish :: Integer -> Day -> Day
putFish daysLeft (Day d0 d1 d2 d3 d4 d5 d6 d7 d8)
  = case daysLeft of
      0 -> (Day (d0+1) d1    d2    d3    d4    d5    d6    d7    d8)
      1 -> (Day d0    (d1+1) d2    d3    d4    d5    d6    d7    d8)
      2 -> (Day d0     d1   (d2+1) d3    d4    d5    d6    d7    d8)
      3 -> (Day d0     d1    d2   (d3+1) d4    d5    d6    d7    d8)
      4 -> (Day d0     d1    d2    d3   (d4+1) d5    d6    d7    d8)
      5 -> (Day d0     d1    d2    d3    d4   (d5+1) d6    d7    d8)
      6 -> (Day d0     d1    d2    d3    d4    d5   (d6+1) d7    d8)
      7 -> (Day d0     d1    d2    d3    d4    d5    d6   (d7+1) d8)
      8 -> (Day d0     d1    d2    d3    d4    d5    d6    d7   (d8+1))

sumDay :: Day -> Integer
sumDay (Day d0 d1 d2 d3 d4 d5 d6 d7 d8) = d0 + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8

parse2 = foldl (flip putFish) emptyDay . parse1

part2 = do
  input <- do
        currentDir <- getCurrentDirectory
        txt <- readFile $ currentDir ++ "/src/inputs/" ++ "6.txt"
        return $ parse2 txt
  let generations = iterate tick2 input

  print $ sumDay $ generations !! 256
