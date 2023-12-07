{-# LANGUAGE TypeApplications #-}

module Days.Day5 (part1, part2) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Lib

type MapEdge = (String, String)

data Range = Range
    { destinationRange :: Int
    , sourceRange :: Int
    , rangeLength :: Int
    }
    deriving (Show)

data Almanak = Almanak
    { almanakSeeds :: [Int]
    , almanakMaps :: M.Map MapEdge [Range]
    }
    deriving (Show)

getInput = parseInput <$> loadInput 5

parseInput :: String -> Almanak
parseInput xs = Almanak seeds blocks
  where
    (seedsInput : inputBlocks) = splitOn [""] $ lines xs
    blocks = M.fromListWith (++) $ map parseBlock inputBlocks
    seeds = map (read @Int) $ words $ drop (length "seeds: ") $ concat seedsInput

parseBlock :: [String] -> (MapEdge, [Range])
parseBlock xs = (header, map parseRange rangesRaw)
  where
    (headerRaw : rangesRaw) = xs
    [headerFrom, headerTo] = splitOn "-to-" $ takeWhile (/= ' ') headerRaw
    header = (headerFrom, headerTo)
    parseRange line = Range dest source len
      where
        [dest, source, len] = map read $ words line

part1 = undefined

part2 = undefined
