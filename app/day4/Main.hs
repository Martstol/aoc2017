module Main where

import Day4

main :: IO ()
main = do
    content <- readFile "res/day4.txt"
    print (solvePart1 content)
    print (solvePart2 content)