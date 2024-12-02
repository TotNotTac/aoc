module Main where

import Control.Monad (forM_)
import qualified Days.Day01 as Day01
import qualified Days.Day02 as Day02
import Text.Printf (printf)
import Util (Day (..))

days :: [IO Day]
days =
    [ Day01.main
    , Day02.main
    ]

main :: IO ()
main = do
    forM_ days $ \(m) -> do
        (Day day r) <- m
        printf "Day %d - result: %s \n" day (show r)
