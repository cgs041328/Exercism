module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List(sortOn)

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor = safeHead. reverse $ palindromes minFactor maxFactor

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor = safeHead $ palindromes minFactor maxFactor


palindromes :: Integer -> Integer -> [(Integer, (Integer, Integer))]
palindromes minFactor maxFactor = sortOn fst $ [(z, (x, y)) | x <- [minFactor..maxFactor], y <- [x..maxFactor], let z = x * y, isPalidrome z]

isPalidrome :: Integer -> Bool
isPalidrome n = s == reverse s
    where s = show n

safeHead :: [(Integer, (Integer, Integer))] -> Maybe (Integer, [(Integer, Integer)])
safeHead []    = Nothing
safeHead ((x,y):xs) = Just $ (x, y:ys)
    where  ys = fmap (\(_,y') -> y') $ takeWhile (\(x',_) -> x' == x) xs