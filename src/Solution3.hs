
module Solution3 where

import Data.List
import Data.Ord

testInput = "00100\n\
\11110\n\
\10110\n\
\10111\n\
\10101\n\
\01111\n\
\00111\n\
\11100\n\
\10000\n\
\11001\n\
\00010\n\
\01010"

bitsToInteger :: [Integer] -> Integer
bitsToInteger xs
  = sum $ zipWith bitToDecimal (reverse xs) [0..]
  where bitToDecimal b pos = b * (2^pos)


part1 :: [String] -> Integer
part1 input = gamma * epsilon
  where mostCommonAtPos i
          = read
          $ take 1
          $ maximumBy (comparing length)
          $ groupBy (==)
          $ sort
          $ map (!!i)
          $ input
        gammaBits = map mostCommonAtPos [0..lineLength - 1]
        gamma = bitsToInteger $ gammaBits
        epsilon = bitsToInteger $ map (\b -> (b - 1) * (-1)) gammaBits
        lineLength = length $ head input


part2 :: [String] -> [String]
part2 input =
  filter (isBitAtPos (mostCommonAtPos 0) 0)
  $ input
  where mostCommonAtPos i = head
                            $ maximumBy (comparing length)
                            $ groupBy (==)
                            $ sort
                            $ map (!!i) input
        isBitAtPos bit pos bits = bit == bits !! pos
        lineLength = length $ head input

test = part2 $ lines testInput

solve3 = do
  print =<< part1 . lines <$> readFile "src/inputs/3.txt"
