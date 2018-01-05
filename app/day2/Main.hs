module Main where

import Day2

main :: IO ()
main = do
    content <- readFile "res/day2.txt"
    print (solvePart1 content)
    print (solvePart2 content)