module Solution1 where

import Data.List
import System.Directory
import Text.Printf

import Lib

input = "199\n\
\200\n\
\208\n\
\210\n\
\200\n\
\207\n\
\240\n\
\269\n\
\260\n\
\263"

input2 = "607\n\
\618\n\
\618\n\
\617\n\
\647\n\
\716\n\
\769\n\
\792"

part1 :: [Int] -> Int
part1 =
  sum
  . map (\x -> if x > 0 then 1 else 0)
  . map (foldl1 (flip (-)) . take 2)
  . filter ((>1) . length)
  . tails

part2 :: [Int] -> Int
part2 =
  sum
  . map (\x -> if x > 0 then 1 else 0)
  . map (foldl1 (flip (-)) . take 2)
  . filter ((>1) . length)
  . tails
  . map (sum . take 3)
  . tails

solve1 = do
  currentDir <- getCurrentDirectory
  input <- readFile $ currentDir ++ "/src/inputs/1.txt"
  printf "Part 1: %d\n" =<< (part1 <$> readLinesFromInputFile "1.txt")
  printf "Part 2: %d\n" =<< (part2 <$> readLinesFromInputFile "1.txt")
