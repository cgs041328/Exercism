module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n = [(x, y, n - x - y) | x <- [1..n `div` 3], y <- [x + 1..(n - x) `div` 2], isPythagorean(x, y, n - x - y)]

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a, b, c) = sqr a + sqr b == sqr c
  where
    sqr = (^(2 :: Integer))
