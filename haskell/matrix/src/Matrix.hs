module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector(Vector)
import qualified Data.Vector as V
import Control.Arrow((&&&))

data Matrix a = Matrix (V.Vector (V.Vector a)) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix ms)
    | V.null ms = 0
    | otherwise = V.length $ V.head ms  

column :: Int -> Matrix a -> Vector a
column x (Matrix ms) = fmap (flipIndex x) ms
    where flipIndex = flip (V.!)

flatten :: Matrix a -> Vector a
flatten (Matrix ms) = V.foldr1 (V.++)  ms

fromList :: [[a]] -> Matrix a
fromList xss = Matrix $ V.fromList $ map V.fromList xss

fromString :: Read a => String -> Matrix a
fromString = fromList. map ((map read).words). lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) matrix = Matrix $ V.map (\x -> V.slice x c flat) (V.fromList [0,c..r*(c-1)])
        where flat = flatten matrix
              

row :: Int -> Matrix a -> Vector a
row x (Matrix ms) = ms V.! x

rows :: Matrix a -> Int
rows (Matrix ms) = V.length ms

shape :: Matrix a -> (Int, Int)
shape = rows &&& cols

transpose :: Matrix a -> Matrix a
transpose matrix = Matrix $ V.generate (cols matrix) (\ri -> column ri matrix)
