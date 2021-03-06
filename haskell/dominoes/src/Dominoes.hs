module Dominoes (chain) where

import Control.Monad (msum)
import Data.Tuple (swap)
import Data.List (delete)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain (x@(l, r):xs) = (x:) <$> chain' l r xs

chain' :: Int -> Int -> [(Int, Int)] -> Maybe [(Int, Int)]
chain' l r [] = if l == r then Just [] else Nothing
chain' l r xs = msum $ map try xs
  where
    try x@(l', r')
      | l' == r = (x:) <$> chain' l r' (delete x xs)
      | r' == r = (swap x:) <$> chain' l l' (delete x xs)
      | otherwise = Nothing
