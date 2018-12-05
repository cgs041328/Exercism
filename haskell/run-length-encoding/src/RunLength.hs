module RunLength (decode, encode) where

import Data.List(group)
import Data.Char(isNumber)

decode :: String -> String
decode []          = []
decode encodedText = replicate n c ++ decode rest'
    where (times, rest) = span isNumber encodedText
          n = if length times > 0 then read times else 1
          c = head rest
          rest' = tail rest

encode :: String -> String
encode text = concatMap (\x -> if length x > 1 then (show. length) x ++ take 1 x else take 1 x) $ group text
