

module Solutions.Solution08 where

import Data.List (transpose)

testInput = "30373\n\
\25512\n\
\65332\n\
\33549\n\
\35390"

showToChar :: Show a => a -> Char
showToChar x = head $ show x

type Forest = [[Int]]

showForest :: Show a => [[a]] -> IO ()
showForest fs = do
  let output = map (map showToChar) fs
  mapM_ putStrLn output
  putStrLn " "

parseInput :: String -> Forest
parseInput
  = map (map (read . (:[])))
  . lines

shiftWest :: Forest -> Forest
shiftWest = map ((++[0]) . tail)

shiftEast :: Forest -> Forest
shiftEast = map (\xs -> 0 : take (length xs - 1) xs)

shiftNorth :: Forest -> Forest
shiftNorth xs = tail xs ++ [replicate (length $ head xs) 0]

shiftSouth :: Forest -> Forest
shiftSouth xs = (replicate (length $ head xs) 0) : (tail xs)

zipMatrixWith = zipWith . zipWith

falseMatrix :: [[a]] -> [[Bool]]
falseMatrix xs = replicate (length xs) (replicate (length $ head xs) False)

markEdges :: [[a]] -> [[Bool]]
markEdges xs = markTopAndBottom $ transpose $ markTopAndBottom $ map (map (const False)) xs
  where horizontalLine = replicate (length $ head xs) True
        markTopAndBottom ys = horizontalLine : (tail $ take (length ys - 1) ys) ++ [horizontalLine]

part1 xs
  = length $ filter id $ concat $ visibleMatrix
  -- = zipMatrixWith (,) visibleMatrix xs
  where checkVisibility shiftFn = zipMatrixWith (>) xs (shiftFn xs)
        visSouth = checkVisibility shiftSouth
        visNorth = checkVisibility shiftNorth
        visEast = checkVisibility shiftEast
        visWest = checkVisibility shiftWest
        visibleMatrix = foldl (zipMatrixWith (||)) (falseMatrix xs) [visNorth, visSouth, visEast, visWest, markEdges xs]

-- $> S8.part1 . S8.parseInput <$> readInputDay 8
