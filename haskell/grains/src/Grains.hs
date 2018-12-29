module Grains (square, total) where

squares :: [Integer]
squares = iterate (*2) 1

square :: Integer -> Maybe Integer
square n
    | n <= 64 && n > 0 = Just $ squares !! (fromIntegral n - 1)
    | otherwise        = Nothing

total :: Integer
total = sum $ take 64 squares 
-- I think 2 ^ 64 - 1 is simpler but less readable.