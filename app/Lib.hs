
module Lib where

import Text.Printf (printf)
import Data.Tuple.Extra ((&&&))

enumerate = zip [0..]

enumerateMap f = map f . enumerate

newtype SolutionFN
  = SolutionFN { runSolutionFN :: String -> (String, String)}

printf s = Text.Printf.printf (s++"\n")

readInputDay day = readFile ("inputs/day"++show day++".txt")

runSolution :: Int -> SolutionFN -> IO ()
runSolution day f = do
  (r1, r2) <- runSolutionFN f <$> readInputDay day
  putStrLn $ "Day "++show day
  putStrLn " Part 1"
  Lib.printf "  %s" (show r1)

  putStrLn " Part 2"
  Lib.printf "  %s" (show r2)
  putStrLn ""

modifyNthElement :: Int -> (a -> a) -> [a] -> [a]
modifyNthElement n f xs
  = (\(i, x) -> if i == n then f x else x) <$> enumerate xs

(&&&) = (Data.Tuple.Extra.&&&)
