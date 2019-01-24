module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))
import Data.List ((\\))

primesUpTo :: Integer -> [Integer]
primesUpTo n =  sieve [2..n]
             where 
                sieve (x:xs) = x : sieve (xs \\ [x,x+x..n])
                sieve [] = []
