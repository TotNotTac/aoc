
module Solutions.Solution05 where

import Lib
import Lib.Parsing
import Data.List (transpose, find)
import Data.Bifunctor (first)
import Data.Maybe

{-
The ship has a giant cargo crane capable of moving crates between stacks. To
ensure none of the crates get crushed or fall over, the crane operator will
rearrange them in a series of carefully-planned steps. After the crates are
rearranged, the desired crates will be at the top of each stack.

The Elves don't want to interrupt the crane operator during this delicate
procedure, but they forgot to ask her which crate will end up where, and they
want to be ready to unload them as soon as possible so they can embark.
-}

data Cell = FullCell Char | EmptyCell
  deriving (Eq)

showLine :: Line -> String
showLine = map (\(FullCell x) -> x)

cellParser :: Parser Cell
cellParser = emptyCellParser <|> fullCellParser
  where
    emptyCellParser = do
      chunk "   " -- 3 Spaces
      return EmptyCell
    fullCellParser = do
      char '['
      c <- letterChar
      char ']'
      return $ FullCell c

type Line = [Cell]

lineParser :: Parser Line
lineParser = do
  cells <- sepBy cellParser (char ' ')
  newline
  return cells

towerParser :: Parser [Line]
towerParser = do
  lines <- many lineParser
  space
  skipMany (lexeme integer)
  return lines

data Instruction = Instruction { instructionMoveCount :: Int
                               , instructionFrom :: Int
                               , instructionTo :: Int
                               }
                 deriving (Show, Eq)

instructionParser :: Parser Instruction
instructionParser = do
  chunk "move "
  i1 <- integer
  chunk "from "
  i2 <- integer
  chunk "to "
  i3 <- integer
  return $ Instruction i1 i2 i3

instructionsParser :: Parser [Instruction]
instructionsParser = do
  r <- sepEndBy instructionParser newline
  return r

inputParser = do
  t <- towerParser
  newline >> newline
  i <- instructionsParser
  return (t, i)

parseInput
  = first (map (filter (/= EmptyCell)) . transpose) -- Transpose the towers so they are grouped right, and filter out the emtpy cells
  . fromJust -- Surely you don't fail
  . parseMaybe inputParser

crateMover :: (Line -> Line) -> [Line] -> [Instruction] -> String
crateMover moveFn tower instructions
  = showLine
  $ map head
  $ filter (not . null)
  $ fst
  $ fromJust
  $ find (null . snd)
  $ iterate (uncurry itt) (tower, instructions)
  where itt tower (i:is)
          = (\t -> (t, is))
          $ modifyNthElement toTarget   (\xs -> (moveFn $ take moveCount (tower !! fromTarget)) ++ xs) -- I had some weird bug here so I had to write the append explicitly for some godforsaken reason
          $ modifyNthElement fromTarget (drop moveCount) tower
            where fromTarget = instructionFrom i - 1
                  toTarget = instructionTo i - 1
                  moveCount = instructionMoveCount i
        itt tower _ = (tower, [])

part1 = uncurry (crateMover reverse)
part2 = uncurry (crateMover id)

solution = SolutionFN $ \input -> let parsed = parseInput input
                                  in (part1 parsed , part2 parsed)

-- $> runSolution 5 S5.solution

