module Main where

import qualified Data.Vector.Unboxed as V

main = do
    input <- getContents
    let trees = map (V.fromList . map (== '#')) (lines input)
    print $ count (treesOnPath trees 3)
    let part2 = map (count . treesOnPath trees) [1, 3, 5, 7]
    print $ product part2 * count (treesOnPath (skip trees) 1)

skip :: [a] -> [a]
skip [] = []
skip [x] = [x]
skip (x:_:xs) = x : skip xs

count :: [Bool] -> Int
count xs = length $ filter id xs

treesOnPath :: [V.Vector Bool] -> Int -> [Bool]
treesOnPath trees step = zipWith (!%) trees (path step)

path :: Int -> [Int]
path step = [0,step..]

(!%) :: V.Unbox a => V.Vector a -> Int -> a
v !% i = v V.! (i `mod` V.length v)