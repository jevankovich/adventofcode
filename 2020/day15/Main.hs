{-# LANGUAGE BangPatterns #-}
module Main where

-- import qualified Data.HashMap.Strict as M
import qualified Data.IntMap as M

input = [6, 19, 0, 5, 7, 13, 1]

main = do
    print $ game input !! (2020 - 1)
    print $ game input !! (30000000 - 1)

game :: [Int] -> [Int]
game = game' M.empty 0 where
    game' _ _ [] = []
    game' m !n [!x] = let x' = n - M.findWithDefault n x m in x : game' (M.insert x n m) (n+1) [x']
    game' m !n ((!x):xs) = x : game' (M.insert x n m) (n+1) xs