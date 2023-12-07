
module Solution3 where

import Data.List
import Data.Ord

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


part2 :: Int -> Bool -> [String] -> String
part2 _ oxygen [x]  = x
part2 12 _ xs  = head xs
part2 pos oxygen xs = part2 (pos+1) oxygen $ filter (\x -> x!!pos == target) xs
  where target = case (oxygen, mostPopularBit pos xs) of
                   (True, b) -> b
                   (False, '1') -> '0'
                   (False, '0') -> '1'

mostPopularBit :: Int -> [String] -> Char
mostPopularBit pos input =
  head
  $ sort
  $ map (!!pos) input

testInput = ["11110", "10110", "10111", "10101", "11100", "10000", "11001"]

-- test = part2 $ ["11110", "10110", "10111", "10101", "11100", "10000", "11001"]

solve3 = do
  print =<< part1 . lines <$> readFile "src/inputs/3.txt"
