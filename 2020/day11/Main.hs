module Main where

import qualified Data.Vector as V
import Data.Maybe (catMaybes, mapMaybe)

main = do
    input <- getContents
    let grid = V.fromList . map V.fromList . lines $ input
    let end = fixedPoint updateGrid grid
    print $ countSeated end
    let end2 = fixedPoint updateGrid2 grid
    print $ countSeated end2
    return ()

countSeated = V.sum . V.map V.sum . (V.map . V.map) (\x -> if x == '#' then 1 else 0)

fixedPoint f x = let x' = f x in if x == x' then x else fixedPoint f x'

updateGrid g = V.imap (\y -> V.imap (\x v -> update v (neighbors g (x, y)))) g

update '.' _ = '.'
update 'L' adj = if count (=='#') adj == 0 then '#' else 'L'
update '#' adj = if count (=='#') adj >= 4 then 'L' else '#'

updateGrid2 g = V.imap (\y -> V.imap (\x v -> update' v (visible g (x, y)))) g where
    update' '.' _ = '.'
    update' 'L' n = if n == 0 then '#' else 'L'
    update' '#' n = if n >= 5 then 'L' else '#'

neighbors g (x, y) = 
    catMaybes [above >>= (V.!? (x-1)), above >>= (V.!? x), above >>= (V.!? (x+1)),
               level >>= (V.!? (x-1)), level >>= (V.!? (x+1)),
               below >>= (V.!? (x-1)), below >>= (V.!? x), below >>= (V.!? (x+1))] where
    above = g V.!? (y-1)
    level = g V.!? y
    below = g V.!? (y+1)

count p = length . filter p

visible g (x, y) = count first . map (uncurry (getRay g x y)) $ [(1,0), (1,1), (0,1),(-1,1), (-1,0), (-1,-1), (0,-1), (1,-1)] where
    first [] = False
    first ('L':_) = False
    first ('#':_) = True
    first (_:xs) = first xs

getRay g x y dx dy = mapMaybe index2 [(x + dx * i, y + dy * i) | i <- [1..limit]] where
    index2 (x, y) = (g V.! y) V.!? x
    limit | dy == 0, dx > 0 = (V.length (g V.! y) - x - 1) `div` dx
          | dy == 0         = x `div` (-dx)
          | dy > 0          = (V.length g - y - 1) `div` dy
          | otherwise       = y `div` (-dy)


        