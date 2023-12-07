
module Solutions.Solution07 where

import Lib
import Lib.Parsing

testInput2 = "$ cd /\n\
\$ ls\n\
\dir a\n\
\14848514 b.txt\n\
\8504156 c.dat\n\
\dir d\n\
\$ cd a\n\
\$ ls\n\
\dir e\n\
\29116 f\n\
\2557 g\n\
\62596 h.lst\n\
\$ cd e\n\
\$ ls\n\
\584 i\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd d\n\
\$ ls\n\
\4060174 j\n\
\8033020 d.log\n\
\5626152 d.ext\n\
\7214296 k"

testInput = "$ cd /\n\
\$ ls\n\
\dir a\n\
\14848514 b.txt\n\
\8504156 c.dat\n\
\dir d"

data Path = Dir String [Path] | File String Int | Bleh
  deriving (Show, Eq)

emptyDirP :: Parser Path
emptyDirP = do
  string "dir "
  many printChar
  optional eol
  return Bleh

fileParser :: Parser Path
fileParser = do
  s <- integer
  space
  n <- many printChar
  optional eol
  return $ File n s

pathP :: Parser Path
pathP = do
  string "$ cd "
  d <- many printChar <* eol
  string "$ ls" <* eol
  files <- many (fileParser <|> emptyDirP)
  return $ Dir d (files)


-- $> Parsing.parseTest (Parsing.many (S7.pathP <* Parsing.eol)) S7.testInput2
