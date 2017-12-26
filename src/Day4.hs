module Day4 (solvePart1) where

import Data.List (nub)

validPassphrase :: String -> Bool
validPassphrase phrase = let
        w = words phrase
    in nub w == w

numberOfValidPassphrases :: [String] -> Int
numberOfValidPassphrases phrases = length (filter validPassphrase phrases)

solvePart1 :: String -> Int
solvePart1 input = numberOfValidPassphrases (lines input)