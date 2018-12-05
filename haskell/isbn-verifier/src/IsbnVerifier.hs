module IsbnVerifier (isbn) where

import Data.Char(isNumber, digitToInt)

isbn :: String -> Bool
isbn s = if (length s' == 10 && all isNumber (take 9 s'))
         then sum' `mod` 11 == 0
         else False
    where s' = filter (\x -> isNumber x || x == 'X') s
          sum' =sum. zipWith (*) (reverse [1..10]) $ map (\x -> if x == 'X' then 10 else digitToInt x) s'

