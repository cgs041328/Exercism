module Change (findFewestCoins) where

import Data.List(sortOn)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins = safeHead. sortOn length $ findPossibleCoins target coins
    where safeHead []    = Nothing
          safeHead (x:_) = Just x

findPossibleCoins :: Integer -> [Integer] -> [[Integer]]
findPossibleCoins _ [] = []
findPossibleCoins target coins@(c:cs) = coinsWithoutC ++ coinsWithC
        where  coinsWithC
                | c > target  = [] 
                | c == target = [[c]]
                | otherwise   = fmap (c:) (findPossibleCoins (target - c) coins)
               coinsWithoutC = findPossibleCoins target cs