{-# LANGUAGE LambdaCase #-}

module Solution2 where

import Data.List
import System.Directory
import Data.Bifunctor (first, second)
import Control.Monad.State
import Text.Printf

input :: IO String
input = do
  currentDir <- getCurrentDirectory
  readFile (currentDir ++ "/src/inputs/2.txt")

testInput = "forward 5\n\
\down 5\n\
\forward 8\n\
\up 3\n\
\down 8\n\
\forward 2"

parseInput :: String -> [(String, Integer)]
parseInput
  = map (second read . span (/= ' '))
  . lines

sumV2 (x1, y1) (x2, y2) = (x1+x2,y1+y2)

part1 =
  uncurry (*)
  . foldl sumV2 (0,0)
  . map (\case
            ("forward", dx) -> (dx, 0)
            ("up", dy) -> (0, -dy)
            ("down", dy) -> (0, dy))

type V2 = (Integer, Integer)
type Aim = Integer

driveSubmarine :: Aim -> V2 -> [V2] -> V2
driveSubmarine _ pos [] = pos
driveSubmarine aim (x, y) ((delta, dAim):rest)
  = driveSubmarine (aim+dAim) (x+delta, y + delta * aim) rest


part2 =
  uncurry (*)
  . driveSubmarine 0 (0,0)
  . map (\case
            ("forward", delta) -> (delta, 0)
            ("up", dAim) -> (0, -dAim)
            ("down", dAim) -> (0, dAim))

solve2 = do
  inp <- parseInput <$> input
  printf "Part 1: %d\n" $ part1 inp
  printf "Part 2: %d\n" $ part2 inp
