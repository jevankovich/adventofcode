module Main where

import qualified Data.IntMap as M

import Data.Bits ((.&.), (.|.), xor, shiftR, shiftL)
import Data.List (foldl')

import Text.Parsec
import Text.Parsec.Char

type Parser = Parsec String ()

main = do
    input <- getContents
    case parse (many instruction) "stdin" input of
        Left e -> print e
        Right ins -> do
            let Computer _ _ mem = foldl' update (Computer 0 0 M.empty) ins
            print $ sum mem

            let Computer2 _ _ mem = foldl' update2 (Computer2 0 0 M.empty) ins
            print $ sum mem
    return ()

data Instruction = SetMask !Int !Int !Int | SetMem !Int !Int deriving (Show)
data Computer = Computer !Int !Int (M.IntMap Int) deriving (Show)

allOnes = fromBits $ replicate 36 True

update :: Computer -> Instruction -> Computer
update (Computer _ _ mem) (SetMask o a _) = Computer o a mem
update (Computer o a mem) (SetMem i x) = Computer o a (M.insert i ((x .|. o) .&. (a `xor` allOnes)) mem)

data Computer2 = Computer2 !Int !Int (M.IntMap Int) deriving (Show)

update2 :: Computer2 -> Instruction -> Computer2
update2 (Computer2 _ _ mem) (SetMask o _ f) = Computer2 o f mem
update2 (Computer2 o f mem) (SetMem i x) = Computer2 o f (foldl' (\m k -> M.insert k x m) mem ks) where
    ks :: [Int]
    ks = [(i .|. o) .&. (f `xor` allOnes) .|. c | c <- bitCombinations f]

bitCombinations :: Int -> [Int]
bitCombinations 0 = [0]
bitCombinations 1 = [0, 1]
bitCombinations n | even n    = [x `shiftL` 1 | x <- bitCombinations (n `shiftR` 1)]
                  | otherwise = [x `shiftL` 1 + b | x <- bitCombinations (n `shiftR` 1), b <- [0, 1]]

instruction = char 'm' *> (mask <|> mem) <* endOfLine

mask :: Parser Instruction
mask = do
    string "ask = "
    val <- many1 (oneOf "X10")
    let or = fromBits (map (== '1') val)
    let and = fromBits (map (== '0') val)
    let floating = fromBits (map (== 'X') val)
    return $ SetMask or and floating

mem :: Parser Instruction
mem = do
    string "em["
    loc <- many1 digit
    string "] = "
    val <- many1 digit
    return $ SetMem (read loc) (read val)

fromBits :: (Num a) => [Bool] -> a
fromBits = fromInteger . fromBits' 0 where
    fromBits' :: Integer -> [Bool] -> Integer
    fromBits' n [] = n
    fromBits' n (b:bs) = seq n $ fromBits' ((if b then 1 else 0) + 2 * n) bs