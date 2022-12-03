module Main where

import qualified Solutions.Solution01 as S1
import qualified Solutions.Solution02 as S2
import qualified Solutions.Solution03 as S3
import Lib

main :: IO ()
main = do
  putStrLn "Solutions\n"
  mapM_ (uncurry runSolution) $ zip [1..] [ S1.solution
                                          , S2.solution
                                          , S3.solution
                                          ]
