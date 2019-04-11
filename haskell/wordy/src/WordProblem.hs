{-# LANGUAGE OverloadedStrings #-}
module WordProblem (answer) where

import Data.Attoparsec.Text
import Data.Text (pack)
import Data.List (foldl')

answer :: String -> Maybe Integer
answer = maybeResult . parse answerParser . pack

answerParser :: Parser Integer
answerParser = do
    n <- string "What is " *> signed decimal
    ops <- many' (space *> operation) 
    string "?"
    return (foldl' (\a op-> op a) n ops) 

operation :: Parser (Integer -> Integer)
operation = do
    op <- operator <* space
    n  <- signed decimal
    return (flip op n)    

operator :: Parser (Integer -> Integer -> Integer)
operator = choice [string "plus"          *> pure (+),
                   string "minus"         *> pure (-),
                   string "multiplied by" *> pure (*),
                   string "divided by"    *> pure div]