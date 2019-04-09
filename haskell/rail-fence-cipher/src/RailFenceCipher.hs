module RailFenceCipher (encode, decode) where

import Data.List
import Data.Function

encode :: Int -> [a] -> [a]
encode n s = sortByFst index s
    where index = cycle $ [1..n] ++ reverse [2..n-1]

decode :: Int -> [a] -> [a]
decode n s = sortByFst index s 
    where index = encode n [1..length s]

sortByFst :: [Int] -> [a] -> [a]    
sortByFst index s = fmap snd. concat .groupBy ((==) `on` fst). sortOn fst $ zip index s