module Main where

import qualified Solutions.Solution01 as S1
import qualified Solutions.Solution02 as S2
import qualified Solutions.Solution03 as S3
import qualified Solutions.Solution04 as S4
import qualified Solutions.Solution05 as S5
import qualified Solutions.Solution06 as S6

import Lib
import qualified Lib.Parsing as Parsing

solutions = [ S1.solution
            , S2.solution
            , S3.solution
            , S4.solution
            , S5.solution
            , S6.solution
            ]

main :: IO ()
main = do
  putStrLn "Solutions\n"
  mapM_ (uncurry runSolution) $ zip [1..] solutions

-- $> main
