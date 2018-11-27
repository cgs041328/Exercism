module SecretHandshake (handshake) where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

handshake :: Int -> [String]
handshake n = foldl (\acc (b, action) -> if b == '1' then action acc else acc) [] $ zip reversedBin actions
    where actions = [
            flip (++) ["wink"],
            flip (++) ["double blink"],
            flip (++) ["close your eyes"],
            flip (++) ["jump"],
            reverse ]
          reversedBin = reverse $ showIntAtBase 2 intToDigit n ""
