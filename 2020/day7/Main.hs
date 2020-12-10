{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Tree as T
import qualified Data.Map as M
import qualified Data.Set as S

import Data.List (foldl')
import Control.Applicative (liftA2)
import Control.Monad (join)

import Text.Parsec
import Text.Parsec.Char

type Parser = Parsec String ()

seed = "shiny gold"

main :: IO ()
main = do
    input <- getContents
    relation <- case parse (many clause) "stdin" input of
        Left e -> do 
            print e
            return M.empty
        Right cs -> do
            return $ M.fromList cs
    let inv = inverse . M.map (map snd) $ relation
    let invTrans = transitive inv
    print $ subtract 1 . S.size  <$> S.fromList <$> T.flatten <$> M.lookup seed invTrans
    print $ M.lookup seed $ countBags relation
    return ()

inverse :: (Ord a, Ord b) => M.Map a [b] -> M.Map b [a]
inverse = foldl' inverse' M.empty . M.toList where
    inverse' m (k, vs) = foldl' (\m v -> M.insertWith (++) v [k] m) m vs

transitive :: (Ord a) => M.Map a [a] -> M.Map a (T.Tree a)
transitive m = M.mapWithKey (\k _ -> transitive' k m) m where
    transitive' k m = T.unfoldTree (\k -> (k, concat $ M.lookup k m)) k

-- countBags :: M.Map String [(Int, String)] -> String -> Int
-- countBags m b = countBags' b - 1 where -- -1 since we don't count the root bag
--     countBags' :: String -> Int
--     countBags' b = case M.lookup b m of
--         Nothing -> error $ "Non-existent bag type " ++ b
--         Just bs -> 1 + (sum . map (\(n, b) -> n * countBags' b) $ bs)
-- countBags :: M.Map String [(Int, String)] -> M.Map String (Maybe Int)
countBags :: M.Map String [(Int, String)] -> M.Map String Int
countBags m = M.mapMaybe id m' where 
    m' = M.map (foldl' (liftA2 (+)) (Just 0) . map (\(n, b) -> fmap ((n*) . (1+)) (join $ M.lookup b m'))) m

bag :: Parser String
bag = do
    modifier <- many1 letter
    space
    adjective <- many1 letter
    space
    string "bag"
    optional $ char 's'
    return $ modifier ++ " " ++ adjective

nbags :: Parser (Int, String)
nbags = do
    n <- many1 digit
    space
    b <- bag
    return (read n, b)
    
nbagsList = (string "no other bags" >> return []) <|> (do
    first <- nbags
    rest <- many (string ", " >> nbags)
    return $ first:rest)

clause :: Parser (String, [(Int, String)])
clause = do
    from <- bag
    string " contain "
    bagsList <- nbagsList
    string ".\n"
    return (from, bagsList)
