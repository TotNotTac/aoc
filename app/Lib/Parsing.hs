
module Lib.Parsing
    ( readLinesFromInputFile
    , readFromInputFile
    , lexeme
    , integer
    , csvInts
    , csv
    , pointP
    , Parser
    , module Text.Megaparsec
    , module Text.Megaparsec.Char
    , module L
    , module LS
    , module Text.Printf
    ) where

import qualified Data.List as LS
import System.Directory
import Text.Printf (printf)

-- Parsing Imports
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void


(&) = flip ($)

mapReadLines :: Read a => String -> [a]
mapReadLines = map read . lines

readLinesFromInputFile :: Read a => FilePath -> IO [a]
readLinesFromInputFile f = do
  currentDir <- getCurrentDirectory
  content <- readFile $ currentDir ++ "/src/inputs/" ++ f
  return $ mapReadLines content

readFromInputFile :: FilePath -> IO String
readFromInputFile f = do
  currentDir <- getCurrentDirectory
  readFile $ currentDir ++ "/src/inputs/" ++ f

-- Parsing stuff

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

integer :: Parser Int
integer = lexeme L.decimal

csvInts :: Parser [Int]
csvInts = sepEndBy integer (lexeme $ char ',') <* eol

noComma :: Parser Char
noComma = satisfy (/=',')

csv :: Parser [String]
csv = sepEndBy (some noComma) (lexeme $ char ',') <* eol

pointP :: Parser (Int, Int)
pointP = do
  i1 <- integer <* lexeme (char ',')
  i2 <- integer
  return $ (i1, i2)
