module Days.Day05 where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Control.Monad.State.Strict (MonadState (get), State, evalState, modify)
import Data.List (partition)
import Data.List.Split (splitOn)
import Util (Day (..))

type Page = Int
type PageRule = (Page, Page)
type RuleMap = [PageRule]
type PageLine = [Page]

main :: IO Day
main = do
    input <- parseInput <$> readFile "inputs/5.txt"
    pure $ Day 5 $ uncurry part1 &&& uncurry part2 $ input

parseInput :: String -> (RuleMap, [PageLine])
parseInput input =
    let [rs, ps] = splitOn [""] . lines $ input
        rules = map ((\[a, b] -> (a, b)) . map read . splitOn "|") rs
        pageLines = map (map read . splitOn ",") ps
     in (rules, pageLines)

testLine :: RuleMap -> PageLine -> Bool
testLine rm pl = evalState (go pl) []
  where
    go :: PageLine -> State [Page] Bool
    go [] = pure True
    go (p : pl) = do
        seen <- get
        let cantBeSeen = map snd $ (filter ((== p) . fst)) rm
            passes = all (`notElem` seen) cantBeSeen
        modify (p :)
        if passes
            then go pl
            else pure False

getMiddle :: [a] -> a
getMiddle xs = xs !! (length xs `div` 2)

reorder :: RuleMap -> PageLine -> PageLine
reorder rm = go
  where
    go [] = []
    go (p : pl) =
        let shouldBeInFront = map fst $ filter ((== p) . snd) rm
            (before, rest) = partition (`elem` shouldBeInFront) pl
         in go before <> [p] <> go rest

part1 :: RuleMap -> [PageLine] -> Int
part1 rules pageLines = sum $ do
    pageLine <- pageLines
    guard $ testLine rules pageLine
    pure $ getMiddle pageLine

part2 :: RuleMap -> [PageLine] -> Int
part2 rules pageLines = sum $ do
    pageLine <- pageLines
    guard $ not $ testLine rules pageLine
    let reordered = reorder rules pageLine
    pure $ getMiddle reordered
