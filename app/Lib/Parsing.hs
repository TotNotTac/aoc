
module Lib.Parsing
    ( lexeme
    , integer
    , csvInts
    , csv
    , pointP
    , Parser
    , module Text.Megaparsec
    , module Text.Megaparsec.Char
    ) where

-- Parsing Imports
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void

mapReadLines :: Read a => String -> [a]
mapReadLines = map read . lines

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
  i1 <- integer
  char ','
  i2 <- integer
  return $ (i1, i2)
