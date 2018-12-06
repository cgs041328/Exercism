module Triangle (rows) where

import Data.List(scanl)

rows :: Int -> [[Integer]]
rows 0 = []
rows x = tail $ scanl (\acc n -> pascal acc n 1 []) [] [1..x]
    where pascal acc n i next
            | i == n    = next ++ [1] 
            | i == 1    = pascal acc n (i + 1) (next ++ [1])
            | otherwise = pascal acc n (i + 1) (next ++ [acc !! (i - 2) + acc !! (i - 1)])

