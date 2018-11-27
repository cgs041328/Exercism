module Acronym (abbreviate) where

import Data.Char (toUpper, isAlpha, isUpper)

abbreviate :: String -> String
abbreviate = concatMap filterAcronym. words'

words' :: String -> [String]
words' [] = []
words' s  = case dropWhile isDelimiter s of
                "" -> []
                s' -> w : words' s''
                      where (w, s'') = break isDelimiter s'
        where isDelimiter x = not $ (isAlpha x || x == '\'')

filterAcronym :: String -> String
filterAcronym [] = []
filterAcronym (x : xs) = toUpper x : xs'
    where xs' = if isAcronym xs
                then []
                else filter isUpper xs
          isAcronym = all isUpper


