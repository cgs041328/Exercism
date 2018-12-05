module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isUpper)
import Data.List (isSuffixOf)

responseFor :: String -> String
responseFor xs
  | isSilence xs               = "Fine. Be that way!"
  | isYelling && isQuestion xs = "Calm down, I know what I'm doing!"
  | isYelling                  = "Whoa, chill out!"
  | isQuestion xs              = "Sure."
  | otherwise                  = "Whatever."
    where  isSilence = all isSpace
           isYelling = (not. null) filteredAlpha && all isUpper filteredAlpha
           filteredAlpha = filter isAlpha xs
           isQuestion = isSuffixOf "?". filter (not. isSpace)

