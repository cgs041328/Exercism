module Prime (nth) where

primes :: [Integer]
primes = 2 : primes'
    where isPrime (p:ps) n = p * p > n || n `rem` p /= 0 && isPrime ps n
          primes' = 3 : filter (isPrime primes') [5, 7..]  

nth :: Int -> Maybe Integer
nth n 
    | n <= 0    = Nothing
    | otherwise = Just $ primes !! (n - 1) 
