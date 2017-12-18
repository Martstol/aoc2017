module Day1 (solvePart1, solvePart2) where

import Data.Char

parseInput :: String -> [Int]
parseInput = map digitToInt

solvePart1 :: String -> Int
solvePart1 input = capcha (parseInput input) 1

solvePart2 :: String -> Int
solvePart2 input = capcha (parseInput input) (length input `div` 2)

capcha :: [Int] -> Int -> Int
capcha digits stride = sum [x | (x, y) <- zip digits (drop stride (cycle digits)), x == y]