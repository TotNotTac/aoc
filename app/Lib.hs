{-# LANGUAGE RankNTypes #-}

module Lib where

import Text.Printf (printf)

enumerate = zip [0..]

enumerateMap f = map f . enumerate

newtype SolutionFN a b
  = SolutionFN { runSolutionFN :: String -> (a, b)}

printf s = Text.Printf.printf (s++"\n")

readInputDay day = readFile ("inputs/day"++show day++".txt")

runSolution :: (Show a, Show b) => Int -> SolutionFN a b -> IO ()
runSolution day f = do
  (r1, r2) <- runSolutionFN f <$> readInputDay day
  putStrLn $ "Day "++show day
  putStrLn " Part 1"
  Lib.printf "  %s" (show r1)

  putStrLn " Part 2"
  Lib.printf "  %s" (show r2)
  putStrLn ""
