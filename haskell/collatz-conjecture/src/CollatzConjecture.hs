module CollatzConjecture (collatz) where
    

collatz :: Integer -> Maybe Integer
collatz n
    | n >= 1    = Just $ fromIntegral $ length $ takeWhile (>1) $ iterate step n
    | otherwise = Nothing

step :: Integer -> Integer
step n
  | even n    = n `quot` 2
  | otherwise = n * 3 + 1