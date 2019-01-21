module PrimeFactors (primeFactors) where


primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = primeFactors' n 2
  where
    primeFactors' n f
      | f*f > n        = [n]
      | n `mod` f == 0 = f : primeFactors' (n `div` f) f
      | otherwise      = primeFactors' n (f + 1)
