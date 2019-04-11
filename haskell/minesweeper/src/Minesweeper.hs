module Minesweeper (annotate) where

import Data.List.Split(chunksOf)
import Data.Char

annotate :: [String] -> [String]
annotate []    = []
annotate [[]]    = [[]]
annotate board =  removeMargin. chunksOf marginWidth. fmap (\(index, x) -> if x == '*' then x else toDigit $ adjacency index). zip [0..] $ concatedBoard
    where concatedBoard = concat withMargin
          width = length $ head board
          marginWidth = width + 2
          marginHeight = length board + 2
          adjacency i = length. filter (\x -> concatedBoard !! x == '*'). filter (\x -> x > 0 && x < marginWidth * marginHeight) $ concatMap (\x -> [x - 1, x, x + 1]) [i - marginWidth, i, i + marginWidth]   
          withMargin = margin : (fmap (\r -> ' ' : r ++ " ") board) ++ [margin]
          margin = take marginWidth $ repeat ' '
          removeMargin (b:bs) = fmap (\(r:rs) -> init rs) $ init bs 
          toDigit 0 = ' '
          toDigit x = intToDigit x