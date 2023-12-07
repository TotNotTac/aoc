module Days.Day1 (part1, part2) where

import Data.Char (isDigit)
import Data.Foldable (find)
import Data.List (isPrefixOf, tails)
import Data.Maybe (fromJust, mapMaybe)
import Lib

getInput = lines <$> loadInput 1

solve1 :: String -> Int
solve1 line = read [firstDigit, lastDigit]
 where
  firstDigit = fromJust $ find isDigit line
  lastDigit = fromJust $ find isDigit $ reverse line

part1 :: IO ()
part1 = do
  input <- getInput
  let vals = map solve1 input
  print $ sum vals

-- Part2

parseDigit :: String -> Maybe Char
parseDigit (c : _) | isDigit c = Just c
parseDigit s
  | "one" `isPrefixOf` s = Just '1'
  | "two" `isPrefixOf` s = Just '2'
  | "three" `isPrefixOf` s = Just '3'
  | "four" `isPrefixOf` s = Just '4'
  | "five" `isPrefixOf` s = Just '5'
  | "six" `isPrefixOf` s = Just '6'
  | "seven" `isPrefixOf` s = Just '7'
  | "eight" `isPrefixOf` s = Just '8'
  | "nine" `isPrefixOf` s = Just '9'
  | otherwise = Nothing

parseLine :: String -> [Char]
parseLine =
  mapMaybe parseDigit . tails

solveLine :: [Char] -> Int
solveLine cs = read [head cs, last cs]

part2 :: IO ()
part2 =
  print . sum . map (solveLine . parseLine) =<< getInput
