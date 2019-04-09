module Sublist (sublist) where

import Data.List(isInfixOf)    

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys  
    | sub && super = Just EQ
    | sub          = Just LT
    | super        = Just GT
    | otherwise    = Nothing
        where sub   = isInfixOf xs ys
              super = isInfixOf ys xs 
