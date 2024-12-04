{-# LANGUAGE OverloadedStrings #-}

{- |

I did the input parsing with MegaParsec.  Which I realise
now is complete overkill.  Next time I'll just use regex
parsing.
-}
module Days.Day03 where

import Control.Arrow ((&&&))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, many, parseMaybe, satisfy, some, try, (<|>))
import Text.Megaparsec.Char (char, digitChar, string)
import Util (Day (Day))

data Instruction
    = Mul Int Int
    | DoInstruction
    | DontInstruction
    deriving (Show, Eq)

type Parser = Parsec Void T.Text

intP :: Parser Int
intP = read <$> some digitChar

anyChar :: Parser Char
anyChar = satisfy (const True)

parseMul :: Parser Instruction
parseMul = do
    string "mul("
    a <- intP
    char ','
    b <- intP
    char ')'
    pure (Mul a b)

instructionP :: Parser Instruction
instructionP =
    skipUntil $
        parseMul
            <|> (string "do()" *> pure DoInstruction)
            <|> (string "don't()" *> pure DontInstruction)

skipUntil :: Parser a -> Parser a
skipUntil p = try p <|> (anyChar >> skipUntil p)

instructionsP :: Parser [Instruction]
instructionsP = do
    is <- many (try instructionP)
    skipUntil eof
    pure is

part1 :: [Instruction] -> Int
part1 (Mul a b : rest) = (a * b) + part1 rest
part1 (_ : rest) = part1 rest
part1 [] = 0

part2 :: [Instruction] -> Int
part2 (Mul a b : rest) = (a * b) + part2 rest
part2 (DoInstruction : rest) = part2 rest
part2 (DontInstruction : rest) = part2 $ dropWhile (/= DoInstruction) rest
part2 [] = 0

main :: IO Day
main = do
    input <- T.readFile "inputs/3.txt"
    let instructions = fromJust $ parseMaybe instructionsP input
    pure $ Day 3 $ part1 &&& part2 $ instructions
