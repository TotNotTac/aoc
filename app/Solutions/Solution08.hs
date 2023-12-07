

module Solutions.Solution08 where

import Data.List (transpose)

testInput = "30373\n\
\25512\n\
\65332\n\
\33549\n\
\35390"

showToChar :: Show a => a -> Char
showToChar x = head $ show x

type Forest = [[Int]]

showForest :: Show a => [[a]] -> IO ()
showForest fs = do
  let output = map (map showToChar) fs
  mapM_ putStrLn output
  putStrLn " "

parseInput :: String -> Forest
parseInput
  = map (map (read . (:[])))
  . lines

scanCountHighest :: Int -> [Int] -> Int
scanCountHighest _ [] = 0
scanCountHighest highestSoFar (x:xs)
  | x > highestSoFar = 1 + scanCountHighest x xs
  | otherwise        = 0 + scanCountHighest x xs

part1 xs = sum $ map horizontal (transpose xs) ++ map horizontal xs
  where horizontal ys = scanCountHighest 0 ys
                        + scanCountHighest 0 (reverse ys)
                        + (if head ys == 0 then 1 else 0)
                        + (if (last ys) == 0 then 1 else 0)

-- $> S8.part1 $ S8.parseInput S8.testInput

-- $> S8.scanCountHighest 0 [1,2,1,0,3,0,1,4,4]
