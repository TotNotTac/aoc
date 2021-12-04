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

solve4 = do
    inp <- parseInput <$> input
    printf "Part1: %d\n" $ uncurry part1 inp
