module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum n = square $ sum [1..n]

sumOfSquares :: Integral a => a -> a
sumOfSquares n = sum $ [square x|x <- [1..n] ]

square :: Integral a => a -> a
square a = a * a