module Days.Day3 (part1, part2) where

import Data.Char (isDigit)
import Data.List (transpose)
import Lib

getInput = lines <$> loadInput 3

fillChar = '.'

shiftUp :: [[Char]] -> [[Char]]
shiftUp xs = drop 1 xs ++ [replicate (length $ head xs) fillChar]

shiftDown :: [[Char]] -> [[Char]]
shiftDown xs = replicate (length $ head xs) fillChar : dropLast 1 xs

shiftRight :: [[Char]] -> [[Char]]
shiftRight =
  transpose
    . shiftDown
    . transpose

shiftLeft :: [[Char]] -> [[Char]]
shiftLeft =
  transpose
    . shiftUp
    . transpose

isDigitAndIsSymbol :: Char -> Char -> Bool
isDigitAndIsSymbol a b = isDigit a && not (isDigit b) && (b /= '.')

mapHasAdjacentSymbol :: [[Char]] -> [[(Char, Bool)]]
mapHasAdjacentSymbol xs =
  zip2DWith (,) xs
    $ zip2DWith (||) hasSymbolBottomRight
    $ zip2DWith (||) hasSymbolBottomLeft
    $ zip2DWith (||) hasSymbolTopRight
    $ zip2DWith (||) hasSymbolTopLeft
    $ zip2DWith (||) hasSymbolBelow
    $ zip2DWith (||) hasSymbolAbove
    $ zip2DWith (||) hasSymbolRight hasSymbolLeft
 where
  testOverlap = zip2DWith isDigitAndIsSymbol
  hasSymbolRight = testOverlap xs $ shiftLeft xs
  hasSymbolLeft = testOverlap xs $ shiftRight xs
  hasSymbolAbove = testOverlap xs $ shiftDown xs
  hasSymbolBelow = testOverlap xs $ shiftUp xs
  hasSymbolTopLeft = testOverlap xs $ shiftRight $ shiftDown xs
  hasSymbolTopRight = testOverlap xs $ shiftLeft $ shiftDown xs
  hasSymbolBottomLeft = testOverlap xs $ shiftRight $ shiftUp xs
  hasSymbolBottomRight = testOverlap xs $ shiftLeft $ shiftUp xs

parseNumbers :: [(Char, Bool)] -> [(Int, Bool)]
parseNumbers [] = []
parseNumbers xs = (read digits, or truthValues) : parseNumbers rest
 where
  (digits', rest) = span (isDigit . fst) $ dropWhile (not . isDigit . fst) xs
  digits = map fst digits'
  truthValues = map snd digits'

zip2DWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zip2DWith f = zipWith (zipWith f)

solve1 :: [[Char]] -> Int
solve1 =
  sum
    . map fst
    . concatMap (filter snd . parseNumbers)
    . mapHasAdjacentSymbol

part1 = do
  input <- getInput
  print $ solve1 input -- > 532428

part2 = undefined