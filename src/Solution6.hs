module Solution6 where

import System.Directory

parse :: String -> [Int]
parse = map read . split . takeWhile (/= '\n')
  where
    split [] = []
    split (c:cs)
          | c == ','  = split cs
          | otherwise = [c] : split cs


ageBlowFish :: Int -> [Int]
ageBlowFish x | x == 0 = [6, 8]
ageBlowFish x          = [x-1]

tick = concat . map ageBlowFish

part1 = do
  input <- do
        currentDir <- getCurrentDirectory
        txt <- readFile $ currentDir ++ "/src/inputs/" ++ "6.txt"
        return $ parse txt
  let generations = iterate tick input

  print $ length $ generations !! 80
