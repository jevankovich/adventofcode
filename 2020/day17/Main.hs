module Main where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Grid = S.Set (Int, Int, Int)
type Grid2 = S.Set (Int, Int, Int, Int)

mapToSet :: (Ord i) => M.Map i Bool -> S.Set i
mapToSet = M.keysSet . M.filter id

update :: (Ord i) => (i -> M.Map i a) -> (a -> a -> a) -> (Bool -> a -> Bool) -> S.Set i -> S.Set i
update kernel f p s = mapToSet . M.mapWithKey (p . (`S.member` s)) . M.unionsWith f . map kernel $ S.toList s

neighbors :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbors (x, y, z) = 
    [(x + dx, y + dy, z + 1) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]] ++
    [(x + dx, y + dy, z - 1) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]] ++
    [(x + dx, y + dy, z) | dx <- [-1, 0, 1], dy <- [-1, 1]] ++
    [(x - 1, y, z), (x + 1, y, z)]

neighbors2 :: (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
neighbors2 (x, y, z, w) = [(x + dx, y + dy, z + dz, w + dw) | dx <- d, dy <- d, dz <- d, dw <- d, not (all (==0) [dx, dy, dz, dw])] where
    d = [-1, 0, 1]

kernel i = M.fromList $ zip (neighbors i) (repeat 1)

rule True 2 = True
rule True 3 = True
rule False 3 = True
rule _ _ = False

kernel2 i = M.fromList $ zip (neighbors2 i) (repeat 1)

updateGrid = update kernel (+) rule
updateGrid2 = update kernel2 (+) rule

buildGrid :: String -> Grid
buildGrid = mapToSet . M.fromList . concat . zipWith (\y -> zipWith (\x el -> ((x, y, 0), el)) [0..]) [0..] . (map . map) (=='#') . lines

buildGrid2 :: String -> Grid2
buildGrid2 = mapToSet . M.fromList .  concat . zipWith (\y -> zipWith (\x el -> ((x, y, 0, 0), el)) [0..]) [0..] . (map . map) (=='#') . lines

main = do
    input <- getContents
    let grid = buildGrid input
        evolution = iterate updateGrid grid
    print $ S.size $ evolution !! 6

    let grid2 = buildGrid2 input
        evolution2 = iterate updateGrid2 grid2
    print $ S.size $ evolution2 !! 6