module Raindrops (convert) where

convert :: Int -> String
convert n = case convert' n of
    Just s  -> s
    Nothing -> show n 
    where pling x
            | x `mod` 3 == 0 = Just "Pling" 
            | otherwise      = Nothing
          plang x
            | x `mod` 5 == 0 = Just "Plang" 
            | otherwise      = Nothing
          plong x
            | x `mod` 7 == 0 = Just "Plong" 
            | otherwise      = Nothing
          convert' x = mconcat [pling x, plang x, plong x]