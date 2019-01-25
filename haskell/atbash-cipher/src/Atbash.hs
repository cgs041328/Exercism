module Atbash (decode, encode) where

import Data.Char
import Data.List.Split(chunksOf)
import Data.List(intercalate)

decode :: String -> String
decode = fmap transposeChar.filter isAlphaNum  

encode :: String -> String
encode = intercalate " ". chunksOf 5. fmap (transposeChar. toLower). filter isAlphaNum  

transposeChar :: Char -> Char
transposeChar c
    | isLetter c = chr $ sum' - ord c
    | otherwise  = c
        where sum' = ord 'a' + ord 'z'
