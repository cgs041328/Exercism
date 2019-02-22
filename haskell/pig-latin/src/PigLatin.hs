module PigLatin (translate) where

import Data.List

translate :: String -> String
translate x 
  | elem ' ' x = intercalate " " $ map translate $ words x 
  | stupidTwoLetterClusterCase x = x ++ "ay" 
  | hasThreeLetterConsonant x = drop 3 x ++ take 3 x ++ "ay"
  | hasTwoLetterConsonant x = drop 2 x ++ take 2 x ++ "ay" 
  | isVowel $ head x = x ++ "ay"
  | not $ isVowel $ head x = drop 1 x ++ take 1 x ++ "ay"
  | otherwise = x
    where
        isVowel x = elem x "aeiou"
        hasTwoLetterConsonant x = elem (take 2 x) ["ch", "qu", "th", "rh"]
        hasThreeLetterConsonant x = elem (take 3 x) ["thr", "sch", "squ"]
        stupidTwoLetterClusterCase x = elem (take 2 x) ["yt", "xr"]
