module Main where

import Data.List (sort)
import Data.Maybe (isJust)
import Data.Array.Unboxed
import Data.Ix (range)

main :: IO ()
main = do
    input <- getContents
    let inputs = sort $ map read $ lines input
    let arr = listArray (1, length inputs) inputs :: UArray Int Int
    case findPair arr (\a b -> (a + b) `compare` 2020) of
        Just (l, h) -> do
            print (arr!l, arr!h)
            print $ arr!l * arr!h
        _ -> putStrLn "No result found"
    
    case findTriple arr (\a b c -> (a + b + c) `compare` 2020) of
        (a, b, Just c):_ -> do
            print (arr!a, arr!b, arr!c)
            print $ arr!a * arr!b * arr!c
        _ -> putStrLn "No result found"

findPair :: (IArray a e, Ix i, Num i) => a i e -> (e -> e -> Ordering) -> Maybe (i, i)
findPair arr p = findPair' (bounds arr) where
    findPair' (l, h) | l >= h = Nothing
                     | otherwise = case p (arr!l) (arr!h) of
                         LT -> findPair' (l + 1, h)
                         EQ -> Just (l, h)
                         GT -> findPair' (l, h - 1)

findTriple :: (IArray a e, Ix i, Integral i) => a i e -> (e -> e -> e -> Ordering) -> [(i, i, Maybe i)]
findTriple arr p = filter (\(_,_,x) -> isJust x) [(l, h, binSearch l h ((l + h) `div` 2) arr (p (arr!l) (arr!h))) | l <- ixs, h <- ixs] where
    ixs = range . bounds $ arr
    binSearch l h i arr p | l >= h = Nothing 
                          | i <= l = Nothing
                          | i >= h = Nothing
                          | otherwise = case p (arr!i) of
                              LT -> binSearch i h ((h + i) `div` 2) arr p
                              EQ -> Just i
                              GT -> binSearch l i ((l + i) `div` 2) arr p