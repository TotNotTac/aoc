module Solution7 where

import Lib
import Data.List
import Data.Maybe
import Control.Applicative

part1 = do
  ls <- fromJust . parseMaybe csvInts <$> readFromInputFile "7.txt"
  printf "Part1: %d\n" $ minimum $ map (\a -> sum $ map (diff a) ls) ls

diff :: Num a => a -> a -> a
diff a b = abs $ a - b
