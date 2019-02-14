module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral 0    = []
spiral size = go size 1
    where go 1 start = [[start]]
          go n start = ([start..start + n - 1] :). fmap reverse $ zipWith (flip (:)) matrix' [start + n..start + 2*n - 2]
            where matrix' = reverse $ go (n-1) (start + 2*n - 1) 