module Lib (loadInput, dropLast) where

import Text.Printf (printf)

loadInput :: Int -> IO String
loadInput day = readFile $ printf "inputs/%d.txt" day

dropLast :: Int -> [a] -> [a]
dropLast n xs = take (length xs - n) xs
