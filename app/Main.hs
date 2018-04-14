module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5

import System.Environment

main :: IO ()
main = do
    day <- readDay
    input <- readInput day
    solveDay day input

readDay :: IO Int
readDay = do
    args <- getArgs
    return (read (args !! 0))

readInput :: Int -> IO String
readInput day = readFile ("res/day" ++ (show day) ++ ".txt")

solveDay :: Int -> String -> IO ()
solveDay day input = case day of
    1 -> solve Day1.solvePart1 Day1.solvePart2 input
    2 -> solve Day2.solvePart1 Day2.solvePart2 input
    3 -> solve Day3.solvePart1 Day3.solvePart2 input
    4 -> solve Day4.solvePart1 Day4.solvePart2 input
    5 -> solve Day5.solvePart1 Day5.solvePart2 input
    _ -> fail ("Invalid day: " ++ (show day))

solve :: (String -> Int) -> (String -> Int) -> String -> IO ()
solve part1 part2 input = do
    print (part1 input)
    print (part2 input)
