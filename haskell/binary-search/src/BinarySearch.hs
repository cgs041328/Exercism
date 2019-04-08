module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = find' array x lo hi
        where (lo, hi)  = bounds array


find' :: Ord a => Array Int a -> a -> Int -> Int -> Maybe Int
find' array x lo hi = if lo > hi then Nothing else case (array ! mid) `compare` x  of
        LT -> find' array x (mid + 1) hi
        EQ -> Just mid
        GT -> find' array x lo (hi - 1)
    where mid = (lo + hi) `div` 2
