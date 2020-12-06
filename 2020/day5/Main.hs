module Main where

import qualified Data.Set as S

main :: IO ()
main = do
    input <- getContents
    let ids = S.fromList . map seatId $ lines input
    print $ S.lookupMax ids
    print $ S.map (+1) . S.filter (\x -> case (S.lookupGT x ids) of
        Just y -> y == x + 2
        Nothing -> False) $ ids
    

seatId :: String -> Integer
seatId = bitString . map (\x -> x == 'B' || x == 'R')

bitString :: [Bool] -> Integer
bitString xs = bitString' xs 0 where
    bitString' [] n = n
    bitString' (x:xs) n = bitString' xs (2 * n + toInteger (fromEnum x))