module Main where

import Control.Monad.ST ( ST, runST )
import Data.Array.ST
    ( STUArray, readArray, writeArray, MArray(newArray) )

import Data.Int (Int32)

input = [6, 19, 0, 5, 7, 13, 1]

main = do
    print $ game (2020 - 1) input
    print $ game (30000000 - 1) input

game :: Int32 -> [Int32] -> Int32
game n xs = runST $ do
    arr <- newArray (min 0 (minimum xs), max n (maximum xs)) 0
    go arr n xs 1 where
        go :: STUArray s Int32 Int32 -> Int32 -> [Int32] -> Int32 -> ST s Int32
        go _ _ [] _ = error "Need some seed values"
        go arr limit xs i | i > limit = return $ head xs
                          | otherwise = case xs of
                              [x] -> do
                                  lastSeen <- readArray arr x
                                  writeArray arr x i

                                  let x' = if lastSeen == 0 then 0 else i - lastSeen
                                  go arr limit [x'] (i + 1)
                              (x:xs) -> do
                                  writeArray arr x i
                                  go arr limit xs (i + 1)