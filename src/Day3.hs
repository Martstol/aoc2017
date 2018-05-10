module Day3 (solvePart1, solvePart2) where

import           Data.Complex
import           Data.Map     (Map, (!))
import qualified Data.Map     as Map

createMovementList :: [Complex Double] -> [Int] -> [Complex Double]
createMovementList (f:s:rest) (x:xs) = replicate x f ++ replicate x s ++ createMovementList rest xs

movementList :: [Complex Double]
movementList = createMovementList (iterate (*(0 :+ 1)) (1 :+ 0)) [1..]

positions :: [Complex Double]
positions = scanl (+) (0 :+ 0) movementList

manhattenDistance :: Complex Double -> Int
manhattenDistance c = round (sum (fmap abs c))

solvePart1 :: String -> Int
solvePart1 input = let
        n = read input
    in manhattenDistance (positions !! (n-1))


solvePart2 :: String ->  Int
solvePart2 input = let
        n = read input
    in populateMap n

populateMap :: Int -> Int
populateMap n = let
        foobar (p:ps) m = let
                v = eval p m
            in if v <= n
                then foobar ps (Map.insert p v m)
                else v
    in foobar positions Map.empty

eval :: Complex Double -> Map (Complex Double) Int -> Int
eval c m
    | Map.null m = 1
    | otherwise  = sum (map snd (neighbours c m))

neighbours :: Complex Double -> Map (Complex Double) Int -> [(Complex Double, Int)]
neighbours (a :+ b) m = let
        member k = Map.member k m
        entry k = (k, m ! k)
        neighbourhood = [(a+x) :+ (b+y) | x <- [-1, 0, 1], y <- [-1, 0, 1], x /= 0 || y /= 0]
    in map entry (filter member neighbourhood)

instance (Ord a) => Ord (Complex a) where
   (a :+ b) `compare` (c :+ d)
        | a > c = GT
        | a < c = LT
        | b > d = GT
        | b < d = LT
        | b == d = EQ

