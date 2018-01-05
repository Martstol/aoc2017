module Main where

import Day1

main :: IO ()
main = do
    content <- readFile "res/day1.txt"
    print (solvePart1 content)
    print (solvePart2 content)