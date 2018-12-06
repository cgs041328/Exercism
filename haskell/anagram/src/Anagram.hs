module Anagram (anagramsFor) where

import Data.List(sort)
import Data.Char(toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (\ys -> let xs' = map toLower xs
                                        ys' = map toLower ys 
                                    in xs' /= ys' && sort xs' == sort ys') xss
