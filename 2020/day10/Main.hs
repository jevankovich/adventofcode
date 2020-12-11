module Main where

import Data.List (sort)
import qualified Data.Map as M

main :: IO ()
main = do
    input <- getContents
    let adapters = sort . map read . lines $ input :: [Int]
    print $ uncurry (*) . countOnesThrees . deltas $ adapters
    print $ countPaths adapters
    return ()

deltas xs = deltas' (0:xs) where
    deltas' (x:y:xs) = y - x : deltas' (y:xs)
    deltas' _ = [3]

countOnesThrees = count' (0, 0) where
    count' p [] = p
    count' (ones, threes) (x:xs) = count' (ones + if x == 1 then 1 else 0, threes + if x == 3 then 1 else 0) xs

-- countPaths :: [Int] -> Int
countPaths xs = M.lookup (maximum xs) (count' (M.fromList [(0, 1)]) xs) where
    lookup = M.findWithDefault 0
    count' m [] = m
    count' m (x:xs) = count' (M.insert x (lookup (x - 1) m + lookup (x - 2) m + lookup (x - 3) m) m) xs