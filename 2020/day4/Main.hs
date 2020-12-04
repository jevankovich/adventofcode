module Main where

import Data.Char (isSpace, isDigit)
import Data.List ((\\))

import Text.Parsec ( Parsec, parse, many1, optional, count, (<|>) )
import Text.Parsec.Char ( space, satisfy, char, letter, digit, string, hexDigit )

type Parser = Parsec String ()
main :: IO ()
main = do
    input <- getContents
    case parse (many1 (passport <* optional space)) "stdin" input of
        Right ps -> do
            print $ Main.count isValid ps
            print $ Main.count isValid2 ps
        Left e -> print e

type Passport = [(String, String)]

passport :: Parser Passport
passport = many1 (field <* space)

field :: Parser (String, String)
field = (,) <$> many1 letter <*> (char ':' *> many1 (satisfy (not . isSpace)))

required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValid :: Passport -> Bool
isValid p = null (required \\ map fst p)

isValid2 :: Passport -> Bool
isValid2 p = and [
    null (required \\ map fst p),
    let Just byr = read <$> lookup "byr" p in byr >= 1920 && byr <= 2002,
    let Just iyr = read <$> lookup "iyr" p in iyr >= 2010 && iyr <= 2020,
    let Just eyr = read <$> lookup "eyr" p in eyr >= 2020 && eyr <= 2030,
    let Just (Right (hgt, units)) = parse height "" <$> lookup "hgt" p in
        ((units == "cm") && hgt >= 150 && hgt <= 193) || ((units == "in") && hgt >= 59 && hgt <= 76),
    let Just hcl = parse hairColor "" <$> lookup "hcl" p in 
        case hcl of 
            Right _ -> True
            Left _ -> False,
    let Just ecl = lookup "ecl" p in 
        ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
    let Just pid = lookup "pid" p in
        length pid == 9 && all isDigit pid
    ]

height :: Parser (Int, String)
height = (,) <$> (read <$> many1 digit) <*> (string "cm" <|> string "in" <|> string "")

hairColor :: Parser String
hairColor = char '#' *> Text.Parsec.count 6 hexDigit

count :: (a -> Bool) -> [a] -> Int
count p xs = length $ filter p xs