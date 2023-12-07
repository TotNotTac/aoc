module Solution4 where

import Data.List
import Lib
import Data.List.Split
import System.Directory
import Text.Printf

(&) = flip ($)

data Cell a
  = Unmarked a
  | Marked a
    deriving (Show)

type Card = [[Cell Int]]

crossFromCard :: Int -> Card -> Card
crossFromCard n card
    = card & map (map (\case
                    (Unmarked v) | v == n -> Marked v
                    x                     -> x))

isMarked :: Cell a -> Bool
isMarked (Marked _) = True
isMarked _          = False

isBingo :: Card -> Bool
isBingo card = bingoRow card || bingoColumn card
  where bingoRow =
          foldl (||) False
          . map (\row -> foldl (&&) True
                  $ map isMarked row)
        bingoColumn = bingoRow . transpose

parseInput :: String -> ([Card], [Int])
parseInput input = (bingoCards, numbers)
  where (numbers':bingoCards') = filter (/=[""])
            $ groupBy (\a b -> a /= "" && b /= "")
            $ lines input
        bingoCards = map (map (map (Unmarked . read) . words)) bingoCards'
        numbers = map read $ splitOn "," $ head numbers'

allUnmarked :: Card -> [Int]
allUnmarked = map (\(Unmarked v) -> v) . concat . map (filter (not . isMarked))


input = do
  currentDir <- getCurrentDirectory
  readFile $ currentDir ++ "/src/inputs/" ++ "4.txt"

part1 :: [Card] -> [Int] -> Int
part1 cards [] = error "No draws left"
part1 cards (draw:draws) = case winningCards of
                           [card] -> draw * (sum $ allUnmarked card)
                           _ -> part1 newCards draws
  where newCards = map (crossFromCard draw) cards
        winningCards = filter isBingo newCards

testInput = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
\\n\
\22 13 17 11  0\n\
\ 8  2 23  4 24\n\
\21  9 14 16  7\n\
\ 6 10  3 18  5\n\
\ 1 12 20 15 19\n\
\              \n\
\ 3 15  0  2 22\n\
\ 9 18 13 17  5\n\
\19  8  7 25 23\n\
\20 11 10 24  4\n\
\14 21 16 12  6\n\
\              \n\
\14 21 17 24  4\n\
\10 16 15  9 19\n\
\18  8 23 26 20\n\
\22 11 13  6  5\n\
\ 2  0 12  3  7"

part2 :: [Card] -- Cards
  -> [Int] -- Calls
  -> Int -- Previous draw
  -> Int -- Score
part2 cards [] _ = error "No draws left"
part2 cards (draw:draws) _ = case losingCards of
                           [card] -> draw * (sum $ allUnmarked card)
                           _ -> if length losingCards /= 0
                                then part2 newCards draws draw
                                else draw
  where newCards = map (crossFromCard draw) cards
        losingCards = filter (not . isBingo) newCards
-- part2 [c] _ prevDraw = prevDraw * (sum $ allUnmarked c)

solve4 = do
    inp <- parseInput <$> input
    printf "Part1: %d\n" $ uncurry part1 inp
