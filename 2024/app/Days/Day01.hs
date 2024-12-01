{-# LANGUAGE TupleSections #-}

module Days.Day01 where

import Control.Arrow ((&&&))
import Data.List (sort)
import qualified Data.Map as M

main :: IO ()
main = do
  input <- formatData <$> readFile "inputs/1.txt"
  print $ uncurry part1 &&& uncurry part2 $ input

formatData :: String -> ([Int], [Int])
formatData = unzip . map ((\[x, y] -> (x, y)) . map read . words) . lines

diff :: (Num a) => a -> a -> a
diff x y = abs $ x - y

part1 :: [Int] -> [Int] -> Int
part1 xs ys = sum $ zipWith diff (sort xs) (sort ys)

part2 :: [Int] -> [Int] -> Int
part2 xs ys = sum $ map (\x -> maybe 0 (* x) $ M.lookup x countMap) xs
 where
  countMap :: M.Map Int Int
  countMap = M.fromListWith (\a b -> a + b) (map (,1) ys)