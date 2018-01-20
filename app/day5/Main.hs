module Main where

import Day5

main :: IO ()
main = do
    content <- readFile "res/day5.txt"
    print (solvePart1 content)
    print (solvePart2 content)