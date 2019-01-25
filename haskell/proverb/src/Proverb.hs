module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite xs = unlines head' ++ tail'
    where l    = length xs
          head' = zipWith (\a b -> "For want of a " ++ a ++ " the " ++ b ++ " was lost." ) (take (l - 1) xs) (tail xs)
          tail' = "And all for the want of a " ++ head xs ++ "." 
