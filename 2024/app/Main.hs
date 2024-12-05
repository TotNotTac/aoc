module Main where

import Control.Monad (forM_)
import qualified Days.Day01 as Day01
import qualified Days.Day02 as Day02
import qualified Days.Day03 as Day03
import qualified Days.Day04 as Day04
import qualified Days.Day05 as Day05
import Text.Printf (printf)
import Util (Day (..), runDay)

days :: [IO Day]
days =
    [ Day01.main
    , Day02.main
    , Day03.main
    , Day04.main
    , Day05.main
    ]

main :: IO ()
main = do
    forM_ days $ \(m) -> do
        (day, result) <- runDay <$> m
        printf "Day %d - result: %s \n" day result
