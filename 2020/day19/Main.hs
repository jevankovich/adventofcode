module Main where

import qualified Data.Set as S

import Text.Parsec
import Data.List.Split
import Data.List
import Debug.Trace
import Data.Functor.Classes
import Control.Monad

type Parser = Parsec String ()

data Rule a = Lit Char | Sequence [a] | Alternate [a] [a] deriving Show
type Production = (Int, Rule Int)

instance Functor Rule where
    fmap f (Lit c) = Lit c
    fmap f (Sequence rs) = Sequence $ map f rs
    fmap f (Alternate l r) = Alternate (map f l) (map f r)

newtype Fix a = Fix {unFix :: a (Fix a)}

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

ana :: (Functor f) => (a -> f a) -> a -> Fix f
ana f = let a = Fix . fmap a . f in a


productionList = many (production <* optional (char '\n'))

production :: Parser Production
production = do
    binder <- read <$> natural
    string ": "
    rhs <- rule
    return (binder, rhs)

rule = lit <|> alternate

lit = Lit <$> (char '"' *> anyChar <* char '"')

alternate = do
    l <- Main.sequence
    (Alternate l <$> (string "| " *> Main.sequence)) <|> return (Sequence l)

sequence = map read <$> many (natural <* optional (char ' '))
    
natural = many1 digit

build m = ana (`lookup'` m) 0 where
    lookup' y xs = case lookup y xs of
        Nothing -> error "missing key"
        Just z -> z

language :: Fix Rule -> [String]
language = cata go where
    go (Lit c) = [[c]]
    go (Sequence rs) = chain rs
    go (Alternate l r) = chain l ++ chain r
    chain = map concat . Prelude.sequence

match :: Fix Rule -> String -> Bool
match r s = "" `elem` go r s where
    go :: Fix Rule -> String -> [String]
    go _ [] = []
    go (Fix (Lit r)) (c:cs) | r == c = [cs]
                            | otherwise = []
    go (Fix (Sequence rs)) cs = chain cs rs
    go (Fix (Alternate l r)) cs = chain cs l ++ chain cs r
    chain cs = foldl' (\cs r -> concatMap (go r) cs) [cs]

count p = length . filter p

main = do
    input <- getContents
    let [rules, strings] = splitOn "\n\n" input
    case parse productionList "stdin" rules of
        Left e -> print e
        Right ruleMap -> do
            let ruleTree = build ruleMap
            -- let lang = S.fromList . language $ ruleMap
            -- print $ Main.count (`S.member` lang) . lines $ strings
            print $ Main.count (match ruleTree) . lines $ strings
            
            let ruleTree2 = build ((8, Alternate [42] [42, 8]):(11, Alternate [42, 31] [42, 11, 31]):ruleMap)
            print $ Main.count (match ruleTree2) . lines $ strings