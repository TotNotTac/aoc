{-# LANGUAGE TupleSections #-}

module Days.Day01 where

import Control.Arrow ((&&&))
import Data.List (sort)
import qualified Data.Map as M
import Util (Day (..), diff, unsafeListToTuple)

main :: IO Day
main = do
  input <- formatData <$> readFile "inputs/1.txt"
  pure $ Day 1 $ uncurry part1 &&& uncurry part2 $ input

formatData :: String -> ([Int], [Int])
formatData = unzip . map (unsafeListToTuple . map read . words) . lines

part1 :: [Int] -> [Int] -> Int
part1 xs ys = sum $ zipWith diff (sort xs) (sort ys)

part2 :: [Int] -> [Int] -> Int
part2 xs ys = sum $ map (\x -> maybe 0 (* x) $ M.lookup x countMap) xs
 where
  countMap :: M.Map Int Int
  countMap = M.fromListWith (\a b -> a + b) (map (,1) ys)