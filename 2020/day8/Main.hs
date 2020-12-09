{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Main where

import Data.Maybe (mapMaybe)
import Data.Either (isRight)

import qualified Data.Vector as V
import qualified Data.Set as S

import Text.Parsec

type Parser = Parsec String ()

main :: IO ()
main = do
    input <- getContents
    case parse program "stdin" input of
        Left e -> print e
        Right insns -> do
            let p = V.fromList insns
            print $ findLoop p
            print . filter isRight . map findLoop . candidates $ p

data Machine = Machine { pcReg :: !Int, accReg :: !Int} deriving (Eq, Show)

candidates :: Program -> [Program]
candidates p = p : mapMaybe corrupt (zip [0..] $ V.toList p) where
    corrupt (_, Instruction Acc _) = Nothing
    corrupt (i, Instruction Nop n) = Just $ p V.// [(i, Instruction Jmp n)]
    corrupt (i, Instruction Jmp n) = Just $ p V.// [(i, Instruction Nop n)]

stepMachine :: Program -> Machine -> Maybe Machine
stepMachine p m = if pcReg m < V.length p
    then Just $ runInstruction (p V.! pcReg m) m
    else Nothing

findLoop :: Program -> Either Int Machine
findLoop p = findLoop' S.empty Machine { pcReg = 0, accReg = 0 } where
    step = stepMachine p
    findLoop' s m = case step m of
        Nothing -> Right m
        Just m' | pcReg m' `S.member` s -> Left $ accReg m
                | otherwise -> findLoop' (S.insert (pcReg m') s) m'

data Instruction = Instruction !Opcode !Int
data Opcode = Nop | Acc | Jmp

type Program = V.Vector Instruction

next m = m { pcReg = pcReg m + 1 }
runInstruction instr m = 
    let m'@Machine { accReg, pcReg } = next m in
    case instr of
        Instruction Nop _ -> m'
        Instruction Acc n -> m' { accReg = accReg + n }
        Instruction Jmp n -> m' { pcReg = pcReg + n - 1 }

program = many instruction

instruction :: Parser Instruction
instruction = do
    mnemonic <- many1 letter
    space
    arg <- argument
    endOfLine
    return $ case mnemonic of
        "nop" -> Instruction Nop arg
        "acc" -> Instruction Acc arg
        "jmp" -> Instruction Jmp arg

argument :: Parser Int
argument = do
    optional $ char '+'
    neg <- optionMaybe $ char '-'
    arg <- many1 digit
    return $ case neg of
        Just _ -> -read arg
        Nothing -> read arg