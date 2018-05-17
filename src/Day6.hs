{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Day6 (solvePart1, solvePart2) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.MArray
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Hashable
import qualified Data.HashTable.Class     as Ht
import qualified Data.HashTable.ST.Cuckoo as Cuckoo
import           Data.List                (foldl')
import           Data.STRef

type Banks = Hashed (UArray Int Int)
type History s = Cuckoo.HashTable s Banks Int
type MBanks s = STUArray s Int Int

getLength :: (MArray a e m, Num i, Ix i) => a i e -> m i
getLength array = do
    (start, end) <- getBounds array
    return (end + 1 - start)

parseInput :: String -> ST s (MBanks s)
parseInput input = let
    content = map read (words input)
    bounds = (0, length content - 1)
 in newListArray bounds content

fromMutableBanks :: MBanks s -> ST s Banks
fromMutableBanks mbanks = hashed <$> freeze mbanks

maxBank :: MBanks s -> ST s Int
maxBank banks = do
    maxIndex <- newSTRef 0
    (start, end) <- getBounds banks
    forM_ [start..end] $ \i -> do
        j <- readSTRef maxIndex
        a <- readArray banks i
        b <- readArray banks j
        writeSTRef maxIndex (if a > b then i else j)
    readSTRef maxIndex

redistribute :: MBanks s -> ST s ()
redistribute banks = do
    i <- maxBank banks
    remaining <- readArray banks i
    writeArray banks i 0
    len <- getLength banks
    forM_ [1..remaining] $ \offset -> do
        let idx = (i + offset) `mod` len
        v <- readArray banks idx
        writeArray banks idx (v+1)

solve :: MBanks s -> ST s (History s)
solve banks = do
    history <- Ht.new
    historyBank <- fromMutableBanks banks
    Ht.insert history historyBank 0
    go history 1
      where
        go history n = do
            redistribute banks
            historyBank <- fromMutableBanks banks
            i <- Ht.lookup history historyBank
            case i of
                Nothing -> do
                    Ht.insert history historyBank n
                    go history (n+1)
                Just _  -> return history

solvePart1 :: String -> Int
solvePart1 input = runST $ do
    banks <- parseInput input
    history <- solve banks
    length <$> Ht.toList history

solvePart2 :: String -> Int
solvePart2 input = runST $ do
    banks <- parseInput input
    history <- solve banks
    hbanks <- fromMutableBanks banks
    len <- length <$> Ht.toList history
    i <- Ht.lookup history hbanks
    case i of
        Nothing -> fail "Cannot find duplicate"
        Just j -> return (len - j)

instance Hashable (UArray Int Int) where
    hashWithSalt salt arr = foldl' hashWithSalt salt (elems arr)

