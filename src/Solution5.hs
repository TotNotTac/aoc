
module Solution5 where

import Data.List
import Data.Bifunctor (bimap, second)
import Data.List.Split
import qualified Data.Map as M
import System.Directory

type V2 = (Int, Int)

data Line
  = Line { from :: V2
        , to :: V2
        }
        deriving (Show)

line = "8,0 -> 0,8"


parseCoord :: String -> V2
parseCoord =
  bimap read read
  . second (drop 1)
  . span (/=',')

dropDiagonalLines :: [Line] -> [Line]
dropDiagonalLines
  = filter (\line -> let (x1, y1) = from line
                         (x2, y2) = to line
                     in x1 == x2 || y1 == y2)

sumV2 (x1, y1) (x2, y2) = (x1+x2, y1+y2)

pointsBetween :: Line -> [V2]
pointsBetween (Line p1@(x1, y1) p2@(x2, y2))
  | p1 == p2  = [p1]
  | otherwise = (x1, y1) : (pointsBetween $ Line (sumV2 p1 stepVector) p2)
  where stepVector = (toDiff x1 x2, toDiff y1 y2)
        toDiff a b
          | a > b     = -1
          | a < b     = 1
          | otherwise = 0


-- Old, very slow implementation
-- countOccurences :: Eq a => [a] -> [(Int, a)]
-- countOccurences [] = []
-- countOccurences (x:xs)
--   = [(1 + (length $ filter (== x) xs), x)] ++ (countOccurences $ filter (/= x) xs)

-- Faster implementation using Data.Map
countOccurences :: (Eq a, Ord a) => M.Map a Int -> [a] -> M.Map a Int
countOccurences countMap [] = countMap
countOccurences countMap (x:xs) = countOccurences (M.insertWith (+) x 1 countMap) xs

part1 = do
  input <- do
        currentDir <- getCurrentDirectory
        readFile $ currentDir ++ "/src/inputs/" ++ "5.txt"
  print $ length
        $ M.filter (>= 2)
        $ countOccurences M.empty
        $ concat
        $ map pointsBetween
        $ dropDiagonalLines
        $ map parseLine
        $ lines input

parseLine :: String -> Line
parseLine line =
  uncurry Line
  $ bimap parseCoord parseCoord
  $ second (dropWhile (flip elem splitChars))
  $ span (not . flip elem splitChars) line
  where splitChars = " ->" :: String
