module Main where

import qualified Solutions.Solution01 as S1
import qualified Solutions.Solution02 as S2
import qualified Solutions.Solution03 as S3

main :: IO ()
main = do
  putStrLn "Solutions\n"
  mapM_ (uncurry runSolution) $ zip [1..] [ S1.solution
                                          , S2.solution
                                          , S3.solution
                                          ]
-- $> main

runSolution :: Int -> (IO String, IO String) -> IO ()
runSolution day (p1, p2) = do
  i <- readFile $ "inputs/day"++show day++".txt"
  putStrLn $ "Day "++show day
  putStrLn " Part 1"
  (putStrLn . ("  "++)) =<< p1

  putStrLn " Part 2"
  (putStrLn . ("  "++)) =<< p2
  putStrLn ""
