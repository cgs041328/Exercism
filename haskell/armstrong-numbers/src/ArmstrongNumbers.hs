module ArmstrongNumbers (armstrong) where

import Data.Char(digitToInt)

armstrong :: Integral a => a -> Bool
armstrong a = (==sum') $ fromIntegral a
    where s    = show $ toInteger a
          l    = length s
          sum' = sum $ fmap ((^l).digitToInt) s