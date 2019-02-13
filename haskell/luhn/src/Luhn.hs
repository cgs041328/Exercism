module Luhn (isValid) where

import Data.Char(digitToInt, isSpace)

isValid :: String -> Bool
isValid xs
    | l <= 1    = False
    | otherwise = evenlyDivisible. sum. handleSecond. reverse . fmap digitToInt $ spaceRemoved
        where handleSecond ns = fmap (\(x, i) -> if (i `mod` 2 == 0) then x * 2 - (if x < 5 then 0 else 9)  else x) $ zip ns [1..]
              l = length spaceRemoved
              spaceRemoved = filter (not. isSpace) xs
              evenlyDivisible = (==0). flip mod 10
