module Main where

import Control.Monad (forM_)
import qualified Days.Day01 as Day01

days :: [(Integer, IO ())]
days =
    [ (1, Day01.main)
    ]

main :: IO ()
main = do
    forM_ days $ \(day, fn) -> do
        print day
        fn
