module Days.Day02 where

import Control.Arrow ((&&&))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Util (Day (..))

type Report = [Int]
type DeltaList = [Int]

main :: IO Day
main = do
  input <- formatData <$> readFile "inputs/2.txt"
  pure $ Day 2 $ part1 &&& part2 $ input

formatData :: String -> [Report]
formatData = map (map read . words) . lines

toDeltaList :: Report -> DeltaList
toDeltaList xs = zipWith (-) xs (drop 1 xs)

isSafeDeltaList :: DeltaList -> Bool
isSafeDeltaList dl = allPositive || allNegative
 where
  allNegative = all isSafeNegative dl
  allPositive = all isSafePositive dl

isSafePositive :: Int -> Bool
isSafePositive x = x >= 1 && x <= 3

isSafeNegative :: Int -> Bool
isSafeNegative x = x <= (-1) && x >= (-3)

part1 :: [Report] -> Int
part1 = length . filter (isSafeDeltaList . toDeltaList)

part2 :: [Report] -> Int
part2 = length . filter (isSafeDeltaList . dampen)

dampen :: Report -> DeltaList
dampen xs =
  let options = do
        ix <- [0 .. length xs]
        pure $ removeIndex ix xs
   in fromMaybe (toDeltaList xs) $ find isSafeDeltaList $ map toDeltaList options

removeIndex :: Int -> [a] -> [a]
removeIndex ix = map snd . filter ((/= ix) . fst) . zip [0 ..]