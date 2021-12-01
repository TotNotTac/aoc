

module Main where

import Text.Printf

import Solutions

days = zip [1..] [ solve1
                 ]

main = do
  printf "What day would you like to run? (0/%d): \n" (length days)
  d <- readLn
  let s = lookup d days

  case s of
    Nothing -> putStrLn $ "Please enter a day beteen 0 and " ++ (show $ length days)
    Just solver -> do
      printf   "Running day %d\n" (d :: Int)
      putStrLn "... "
      solver

  putStrLn "\n"
  main
