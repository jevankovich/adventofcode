module Main where

import qualified Data.IntSet as IS
import qualified Data.Map as M
import Data.List.Split
import Data.List

type Range = IS.IntSet
type Field = (String, Range)
type Ticket = [Int]

range :: Int -> Int -> Range
range l h = IS.fromDistinctAscList [l..h]

parse :: String -> ([Field], Ticket, [Ticket])
parse s = (map parseField fields, parseTicket mine, map parseTicket tickets) where
    parseTicket = map read . splitOn ","
    parseField s = let [name, ranges] = splitOn ": " s
                       ranges' = splitOn " or " ranges
                       pairs = map (\r -> let [l, h] = splitOn "-" r in (read l, read h)) ranges' in
                           (name, foldMap (uncurry range) pairs)
    [fields, [_, mine], _:tickets] = splitOn [""] . lines $ s 

count p = length . filter p
fixedPoint f x = let x' = f x in if x == x' then x else fixedPoint f x'

correlate :: [Field] -> [Ticket] -> [[String]]
correlate fs ts = [map fst . filter (\(_, s) -> all (`IS.member` s) col) $ fs | col <- cols] where
    cols = transpose ts

countOccurences :: (Ord a) => [a] -> M.Map a Int
countOccurences = go M.empty where
    go m [] = m
    go m (x:xs) = go (M.insertWith (+) x 1 m) xs

collapse :: (Eq a) => [[a]] -> [a]
collapse = map (\[x] -> x) . fixedPoint go where
    go yss = map (`remove` (concat . filter ((==1) . length) $ yss)) yss
    remove y@[_] _ = y
    remove ys fs = filter (`notElem` fs) ys

main = do
    input <- getContents
    let (fields, mine, tickets) = parse input
        allRules = foldMap snd fields
        allTicketValues = concat tickets

    print $ sum . filter (`IS.notMember` allRules) $ allTicketValues

    let validTickets = filter (all (`IS.member` allRules)) tickets
        candidates = correlate fields validTickets
        columnMapping = collapse candidates
        colsOfInterest = map snd . filter (\(n, _) -> "departure" `isPrefixOf` n) $ zip columnMapping mine

    print $ product colsOfInterest