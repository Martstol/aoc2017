module Main where

import Day3

main :: IO ()
main = do
    content <- readFile "res/day3.txt"
    print (solvePart1 content)
    print (solvePart2 content)