module Main where

import Text.Parsec ( digit, many1, Parsec, parse )
import Text.Parsec.Char ( char, endOfLine, space, string, letter )

type Parser = Parsec String ()

main :: IO ()
main = do
    input <- getContents
    case parse (many1 line) "stdin" input of
        Left e -> print e
        Right ls -> do
            print $ count isValid ls
            print $ count isValid2 ls

integer :: Parser Int
integer = read <$> many1 digit

data Line = Line !Int !Int !Char !String deriving Show

line :: Parser Line
line = Line <$> integer <*> (char '-' *> integer) <*> (space *> letter) <*> (string ": " *> many1 letter <* endOfLine)

isValid :: Line -> Bool
isValid (Line l h c s) = let cnt = count (==c) s in cnt >= l && cnt <= h

count :: (a -> Bool) -> [a] -> Int
count p xs = length $ filter p xs

isValid2 :: Line -> Bool
isValid2 (Line l h c s) = (s !! (l - 1) == c) /= (s !! (h - 1) == c)