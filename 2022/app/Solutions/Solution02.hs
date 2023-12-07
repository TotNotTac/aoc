-- |

module Solutions.Solution02 where

import Lib

{-
The Elves begin to set up camp on the beach. To decide whose tent gets to be
closest to the snack storage, a giant Rock Paper Scissors tournament is already
in progress.

Rock Paper Scissors is a game between two players. Each game contains many
rounds; in each round, the players each simultaneously choose one of Rock,
Paper, or Scissors using a hand shape. Then, a winner for that round is
selected: Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock.
If both players choose the same shape, the round instead ends in a draw.
-}

data Play = Scissors | Paper | Rock
  deriving (Show, Eq)

type Round = (Play, Play)

comparePlay :: Play -> Play -> Ordering
comparePlay Rock Paper = LT
comparePlay Rock Scissors = GT
comparePlay Paper Scissors = LT
comparePlay Paper Rock = GT
comparePlay Scissors Paper = GT
comparePlay Scissors Rock = LT
comparePlay _ _ = EQ


instance Read Play where
  readsPrec _ [x]
    | x `elem` "AX" = [(Rock,"")]
    | x `elem` "BY" = [(Paper,"")]
    | x `elem` "CZ" = [(Scissors,"")]

playToInt :: Play -> Int
playToInt Rock     = 1
playToInt Paper    = 2
playToInt Scissors = 3

parseInput1 :: String -> [Round]
parseInput1
  = map (\(x:' ':y:_) -> (read [x],read [y]))
  . lines

part1 :: [Round] -> Int
part1
  = sum
  . map (\(opponent,you) -> calcScore opponent you + playToInt you)

calcScore opponent you = case comparePlay opponent you of
                GT -> 0
                EQ -> 3
                LT -> 6

parseInput2 :: String -> [(Play,Char)]
parseInput2
  = map (\(x:' ':y:_) -> (read [x],y))
  . lines

part2 :: [(Play,Char)] -> Int
part2
  = sum
  . map (\play@(opponent,strat) ->
         let yourPlay = case play of
               -- Lose
               (Rock, 'X') -> Scissors
               (Paper, 'X') -> Rock
               (Scissors, 'X') -> Paper
               -- Draw
               (Rock, 'Y') -> Rock
               (Paper, 'Y') -> Paper
               (Scissors, 'Y') -> Scissors
               -- Win
               (Rock, 'Z') -> Paper
               (Paper, 'Z') -> Scissors
               (Scissors, 'Z') -> Rock
        in
        calcScore opponent yourPlay + playToInt yourPlay
        )

solution = SolutionFN $ (show . part1 . parseInput1) &&& (show . part2 . parseInput2)

-- $> runSolutionFN S2.solution <$> readInputDay 2
