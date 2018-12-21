module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ [x | x <- [1..limit - 1], isMultiples x factors']
    where isMultiples x = any (\divisor -> x `rem` divisor == 0)
          factors'      = filter (/=0) factors 
