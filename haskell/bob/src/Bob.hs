module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isUpper)
import Data.List (isPrefixOf, dropWhile)
import Control.Arrow ((&&&))

responseFor :: String -> String
responseFor xs
  | isSilence xs                  = "Fine. Be that way!"
  | isYelling xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | isYelling xs                  = "Whoa, chill out!"
  | isQuestion xs                 = "Sure."
  | otherwise                     = "Whatever."
    where  isSilence = all isSpace
           isYelling ys = ((not.null &&& all isUpper) $ filter isAlpha ys) == (True, True)
           isQuestion = isPrefixOf "?". dropWhile isSpace. reverse

