module Main where

import Text.Parsec
import Data.List ( foldl1' )
import Control.Applicative ( liftA2 )

type Parser = Parsec String ()

data Expr a = Lit a | Sum (Expr a) (Expr a) | Product (Expr a) (Expr a) deriving (Show, Eq)

expr :: (Read a) => Parser (Expr a)
expr = do
    l <- term
    spaces
    expr1 l <|> return l

expr1 :: (Read a) => Expr a -> Parser (Expr a)
expr1 l = do
    op <- oneOf "+*"
    spaces
    r <- term
    spaces
    let sub = case op of
                '+' -> Sum l r
                '*' -> Product l r
    expr1 sub <|> return sub

advancedExpr :: (Read a) => Parser (Expr a)
advancedExpr = do
    l <- sumE
    spaces
    product1 l <|> return l

product1 :: (Read a) => Expr a -> Parser (Expr a)
product1 l = do    
    char '*'
    spaces
    r <- sumE
    spaces
    let sub = Product l r
    product1 sub <|> return sub

sumE :: (Read a) => Parser (Expr a)
sumE = do
    l <- aTerm
    spaces
    sumE1 l <|> return l

sumE1 :: (Read a) => Expr a -> Parser (Expr a)
sumE1 l = do
    char '+'
    spaces
    r <- aTerm
    spaces
    let sub = Sum l r
    sumE1 sub <|> return sub

term :: (Read a) => Parser (Expr a)
term = parens expr <|> Lit . read <$> natural

aTerm :: (Read a) => Parser (Expr a)
aTerm = parens advancedExpr <|> Lit . read <$> natural

eval :: (Num a) => Expr a -> a
eval (Lit x) = x
eval (Sum l r) = eval l + eval r
eval (Product l r) = eval l * eval r

natural = many1 digit

parens :: Parser a -> Parser a
parens a = char '(' *> a <* char ')'

main = do
    input <- getContents
    let expressions = map (parse expr "stdin") $ lines input :: [Either ParseError (Expr Int)]
        advancedExpressions = map (parse advancedExpr "stdin") $ lines input :: [Either ParseError (Expr Int)]
    print $ foldl1' (liftA2 (+)) . (fmap . fmap) eval $ expressions
    print $ foldl1' (liftA2 (+)) . (fmap . fmap) eval $ advancedExpressions

    print $ (parse advancedExpr "" "1 + 2 * 3 + 4 * 5 + 6" :: Either ParseError (Expr Int))