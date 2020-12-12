module Main where

import Data.List (foldl')

main = do
    input <- getContents
    let directions = map (\(c:d) -> (c, read d)) . lines $ input
    let final = foldl' (uncurry . update) (Ship 0 0 (1, 0)) directions
    print final
    print $ manhattanDistance final

    let final2 = foldl' (uncurry . update2) (Ship 0 0 (10, 1)) directions
    print final2
    print $ manhattanDistance final2
    return ()

data Ship = Ship { east :: !Int, north :: !Int, heading :: !(Int, Int) } deriving (Show)

manhattanDistance s = abs (east s) + abs (north s)

mulHeading (a, b) (c, d) = (a*c - b*d, a*d + b*c)

update s 'N' d = s { north = north s + d }
update s 'S' d = s { north = north s - d }
update s 'E' d = s { east = east s + d }
update s 'W' d = s { east = east s - d }

update s 'L' 90  = s { heading = mulHeading (heading s) ( 0,  1) }
update s 'L' 180 = s { heading = mulHeading (heading s) (-1,  0) }
update s 'L' 270 = s { heading = mulHeading (heading s) ( 0, -1) }
update s 'R' 90  = s { heading = mulHeading (heading s) ( 0, -1) }
update s 'R' 180 = s { heading = mulHeading (heading s) (-1,  0) }
update s 'R' 270 = s { heading = mulHeading (heading s) ( 0,  1) }

update s 'F' d = s { east = east s + d * fst (heading s), north = north s + d * snd (heading s) }

-- Treat heading as waypoint
update2 s@Ship { heading = (e,n) } 'N' d = s { heading = (e, n+d) }
update2 s@Ship { heading = (e,n) } 'S' d = s { heading = (e, n-d) }
update2 s@Ship { heading = (e,n) } 'E' d = s { heading = (e+d, n) }
update2 s@Ship { heading = (e,n) } 'W' d = s { heading = (e-d, n) }

update2 s 'L' 90  = s { heading = mulHeading (heading s) ( 0,  1) }
update2 s 'L' 180 = s { heading = mulHeading (heading s) (-1,  0) }
update2 s 'L' 270 = s { heading = mulHeading (heading s) ( 0, -1) }
update2 s 'R' 90  = s { heading = mulHeading (heading s) ( 0, -1) }
update2 s 'R' 180 = s { heading = mulHeading (heading s) (-1,  0) }
update2 s 'R' 270 = s { heading = mulHeading (heading s) ( 0,  1) }

update2 s 'F' d = s { east = east s + d * fst (heading s), north = north s + d * snd (heading s) }