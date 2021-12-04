module Lib
    ( readLinesFromInputFile
    ) where

import Data.List
import System.Directory
import Text.Printf

(&) = flip ($)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

mapReadLines :: Read a => String -> [a]
mapReadLines = map read . lines

readLinesFromInputFile :: Read a => FilePath -> IO [a]
readLinesFromInputFile f = do
  currentDir <- getCurrentDirectory
  content <- readFile $ currentDir ++ "/src/inputs/" ++ f
  return $ mapReadLines content
