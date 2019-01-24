module Series (slices) where

import Data.Char(digitToInt)    

slices :: Int -> String -> [[Int]]
slices n xs = fmap (\i -> (fmap digitToInt. take n. drop i) xs) [0..length xs - n]
