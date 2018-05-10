module Day6 (solvePart1, solvePart2) where

import           Data.List (elemIndex)

type Banks = [Int]

parseInput :: String -> Banks
parseInput input = map read (words input)

pick :: Banks -> Int
pick banks = pick' 0 0 banks
    where
        pick' i _ [] = i
        pick' i n (a:as) = if banks !! i >= a
                             then pick' i (n+1) as
                             else pick' n (n+1) as

redistribute :: Int -> Int -> Banks -> Banks
redistribute _ 0 banks = banks
redistribute i v banks = redistribute (next i banks) (v-1) (update i (+1) banks)

next :: Int -> Banks -> Int
next i banks = mod (i+1) (length banks)

update :: Int -> (Int -> Int) -> Banks -> Banks
update _ _ []     = []
update 0 f (b:bs) = f b:bs
update i f (b:bs) = b:update (i-1) f bs

solve :: Banks -> [Banks] -> Int -> [Banks]
solve banks history i =
    let rbanks = redistribute (next i banks) (banks !! i) (update i (const 0) banks)
     in if rbanks `elem` history
           then rbanks:history
           else solve rbanks (rbanks:history) (pick rbanks)

solvePart1 :: String -> Int
solvePart1 input = let
    banks = parseInput input
    history = solve banks [banks] (pick banks)
    in length history - 1

solvePart2 :: String -> Int
solvePart2 input = let
    banks = parseInput input
    history = solve banks [banks] (pick banks)
    in case elemIndex (head history) (tail history) of
        Just n  -> n+1
        Nothing -> error "Could not find repeated states"

