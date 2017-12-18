module Main where

import Day1 (solvePart1, solvePart2)
import Day2 (solvePart1, solvePart2)

getInputForDay :: Int -> String
getInputForDay n = "res/day" ++ (show n) ++ ".txt"

main :: IO ()
main = do
    content <- readFile (getInputForDay 2)
    print (Day2.solvePart2 content)
