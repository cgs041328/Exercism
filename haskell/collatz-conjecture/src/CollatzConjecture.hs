module CollatzConjecture (collatz) where
    

collatz :: Integer -> Maybe Integer
collatz n
    | n >= 1    = Just $ fromIntegral $ length $ drop 1 $ takeWhile (>=1) $ iterate step n
    | otherwise = Nothing

step :: Integer -> Integer
step n
  | n == 1    = 0
  | even n    = n `div` 2
  | otherwise = n * 3 + 1