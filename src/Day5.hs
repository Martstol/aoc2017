module Day5 (solvePart1, solvePart2) where

import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST

parseInstructions :: String -> ST s (STUArray s Int Int)
parseInstructions input =
    let arrayContent = map read (lines input)
        arrayBounds  = (0, length arrayContent - 1)
    in newListArray arrayBounds arrayContent

follow :: (Int -> Int) -> STUArray s Int Int -> Int -> Int -> ST s Int
follow f x i acc = do
    b <- getBounds x
    if inRange b i
        then do
            v <- readArray x i
            writeArray x i (f v)
            follow f x (i + v) (acc+1)
        else return acc

runInstructions :: (Int -> Int) -> String -> Int
runInstructions f input = runST $ do
    instr <- parseInstructions input
    follow f instr 0 0

solvePart1 :: String -> Int
solvePart1 = runInstructions (+1)

solvePart2 :: String -> Int
solvePart2 = runInstructions (\i -> if i >= 3 then i-1 else i+1)