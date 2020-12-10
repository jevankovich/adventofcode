module Main where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.List (inits, tails)

main = do
    input <- getContents
    let (window:xs) = map read $ lines input :: [Int]
    case findInvalid (windows window xs) of
        Nothing -> putStrLn "Uh oh"
        Just n -> do
            print n
            print $ weakness n xs

windows :: Int -> [Int] -> [(Set.Set Int, Int)]
windows n xs = windows' (Seq.fromList init) (Set.fromList init) (drop n xs) where
    init = take n xs
    windows' _ _ [] = []
    windows' (remove Seq.:<| keep) set (x:xs) = (set, x) : windows' (keep Seq.|> x) (Set.insert x (Set.delete remove set)) xs

containsSum :: Set.Set Int -> Int -> Bool
containsSum s x = not $ Set.null $ Set.intersection s (Set.map (x -) s)

findInvalid :: [(Set.Set Int, Int)] -> Maybe Int
findInvalid xs = snd <$> head' (filter (not . uncurry containsSum) xs) where
    head' [] = Nothing
    head' (x:_) = Just x

findSequence n = head . concatMap findSequence' . tails where
    findSequence' = takeWhile (\ys -> sum ys == n) . dropWhile (\ys -> sum ys < n) . inits

weakness n xs = let ys = findSequence n xs in minimum ys + maximum ys