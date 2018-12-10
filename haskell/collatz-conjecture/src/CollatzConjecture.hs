module CollatzConjecture (collatz) where
    
collatz :: Integer -> Maybe Integer
collatz n
    | n >= 1    = Just $ snd $ collatz' n 0
    | otherwise = Nothing

collatz' :: Integer -> Integer -> (Integer, Integer)
collatz' n counter
  | n == 1 = (n, counter)
  | even n = collatz' (n `div` 2) (counter + 1)
  | otherwise = collatz' (n * 3 + 1) (counter + 1)