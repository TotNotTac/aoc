module Days.Day04 where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Vector.Strict ((!?))
import qualified Data.Vector.Strict as V
import Util (Day (..))

type Matrix a = V.Vector (V.Vector a)
type V2 = (Int, Int)

toMatrix :: [[Char]] -> Matrix Char
toMatrix = V.fromList . map V.fromList

addV2 :: V2 -> V2 -> V2
addV2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

indexMatrix :: Matrix a -> V2 -> Maybe a
indexMatrix mx (x, y) = (mx !? y >>= (!? x))

deltas :: [[V2]]
deltas = [left, right, top, bottom, topleft, topright, bottomleft, bottomright]

left = [(x, 0) | x <- [0, -1 ..]]
right = [(x, 0) | x <- [0 ..]]
top = [(0, y) | y <- [0 ..]]
bottom = [(0, y) | y <- [0, -1 ..]]
topleft = [(-i, i) | i <- [0 ..]]
topright = [(i, i) | i <- [0 ..]]
bottomright = [(i, -i) | i <- [0 ..]]
bottomleft = [(-i, -i) | i <- [0 ..]]

scanXmas :: Matrix Char -> V2 -> [[(Char, V2)]]
scanXmas mx startPoint = do
    d <- zip "XMAS" <$> deltas
    let matchChar (char, dCoord) =
            let coord = addV2 dCoord startPoint
             in indexMatrix mx coord == Just char
    guard $ all matchChar d
    pure d

scanDir :: (Eq a) => Matrix a -> V2 -> [V2] -> [a] -> Bool
scanDir mx p ds target =
    let deltas = zip target $ map (addV2 p) ds
     in all (\(char, pos) -> indexMatrix mx pos == Just char) deltas

part1 :: Matrix Char -> Int
part1 mx = length $ do
    x <- [0 .. V.length mx]
    y <- [0 .. V.length $ V.head mx]
    guard $ indexMatrix mx (x, y) == Just 'X'
    scanXmas mx (x, y)

part2 :: Matrix Char -> Int
part2 mx = length $ do
    x <- [0 .. V.length mx]
    y <- [0 .. V.length $ V.head mx]
    let scan = scanDir mx
    guard $
        (scan (x, y) bottomright "MAS" || scan (x, y) bottomright "SAM")
            && (scan (x, y - 2) topright "MAS" || scan (x, y - 2) topright "SAM")

main :: IO Day
main = do
    input <- toMatrix . lines <$> readFile "inputs/4.txt"
    pure . Day 04 $ part1 &&& part2 $ input