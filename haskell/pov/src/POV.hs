module POV (fromPOV, tracePathBetween) where

import Data.Foldable (asum)
import Data.List (inits, tails)
import Data.Tree (Tree(Node))
import Control.Monad ((<=<))

elemRests :: [a] -> [(a, [a])]
elemRests xs = zip xs $ zipWith (++) (inits xs) (drop 1 $ tails xs)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV goal = go []
  where
    go trace (Node a forest)
        | a == goal = Just $ Node a (trace ++ forest)
        | otherwise = asum . map explore . elemRests $ forest
      where explore (t, ts) = go [Node a (trace ++ ts)] t

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to = go <=< fromPOV from
  where
    go (Node a forest)
        | a == to   = Just [a]
        | otherwise = fmap (a:) . asum . map go $ forest
