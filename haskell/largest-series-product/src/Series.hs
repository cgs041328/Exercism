module Series (Error(..), largestProduct) where

import Data.Char(digitToInt, isDigit)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
        | (not. null) notDigits = Left $ InvalidDigit (head notDigits)
        | size > l || size < 0 = Left InvalidSpan
        | otherwise = Right. fromIntegral. maximum $ fmap (\i -> (sumProduct. take size. drop i) digits) [0..l - size]
    where l = length digits
          sumProduct = product. fmap digitToInt
          notDigits = filter (not. isDigit) digits