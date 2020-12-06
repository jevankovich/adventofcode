module Main where

import qualified Data.Set as S
import Data.List (foldl1')

main :: IO ()
main = do
    input <- getContents
    let groups = (map . map) S.fromList . splitOn "" . lines $ input
    let unions = map (foldl1' S.union) groups
    let intersections = map (foldl1' S.intersection) groups
    print $ sum . map S.size $ unions
    print $ sum . map S.size $ intersections


splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn y = splitOn' [] where
    splitOn' accum [] = [accum]
    splitOn' accum (x:xs) = if x == y
        then accum : splitOn' [] xs
        else splitOn' (accum++[x]) xs