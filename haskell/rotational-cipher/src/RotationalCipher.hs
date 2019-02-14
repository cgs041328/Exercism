module RotationalCipher (rotate) where

import Data.Char

rotate :: Int -> String -> String
rotate n = fmap f
    where rotateChar c loop = head. drop n $ dropWhile (/= c) loop
          lowerLoop = cycle ['a'..'z']  
          upperLoop = cycle ['A'..'Z']
          f c
            | isLower c = rotateChar c lowerLoop
            | isUpper c = rotateChar c upperLoop
            | otherwise = c