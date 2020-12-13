module Main where

import Data.List.Split
import Data.List (sort, foldl1')
import Data.Maybe (isJust)
import Debug.Trace (traceShowId)

main = do
    input <- getContents
    let (time:buses:[]) = lines input

    let time' = read time :: Int
    let buses' = map read . filter (/="x") . splitOn "," $ buses :: [Int]
    let (depart, bus) = firstBus time' buses'
    print $ (depart - time') * bus

    let buses'' = map (uncurry Seq) . zip [0,-1..] . map readBus . splitOn "," $ buses
    print $ (map (offset . foldl1' (<>)) examples) == exampleResults
    print $ foldl1' (<>) buses''

examples = map (map (uncurry Seq) . zip [0,-1..]) [
    [17, 1, 13, 19],
    [67, 7, 59, 61],
    [67, 1, 7, 59, 61],
    [67, 7, 1, 59, 61],
    [1789, 37, 47, 1889]]

exampleResults = [3417, 754018, 779210, 1261476, 1202161486]

firstBus :: Int -> [Int] -> (Int, Int)
firstBus t = head . sort . map (\b -> ((((t `div` b) + 1) * b, b)))

readBus :: String -> Integer
readBus "x" = 1
readBus s = read s

data Seq = Seq { offset :: !Integer, step :: !Integer } deriving (Show, Eq)

instance Semigroup Seq where
    Seq { offset = a, step = m } <> Seq { offset = b, step = n } = 
        Seq { offset = head [ i * m + a | i <- [0..], (i * m + a) `mod` n == b `mod` n], step = m * n }
                             

