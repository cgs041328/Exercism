module Transpose (transpose) where

import qualified Data.List (transpose)

transpose :: [String] -> [String]
transpose = Data.List.transpose . padStrings
  where
    padStrings = zipWith padRight =<< scanr1 max. fmap length
    padRight n s = s ++ replicate (n - length s) ' '


           