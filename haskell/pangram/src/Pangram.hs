module Pangram (isPangram) where

import Data.Char(isAlpha, toLower)
import Data.List(sort, nub)
    
    
isPangram :: String -> Bool
isPangram text = text' == alphabet
    where alphabet = ['a'..'z']
          text' = (sort. nub) $ map toLower $ filter isAlpha text
