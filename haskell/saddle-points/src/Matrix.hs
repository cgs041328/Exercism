module Matrix (saddlePoints) where

import Data.Array (Array, assocs, bounds)
import Data.List(maximumBy, minimumBy, intersect)
import Data.Ord(comparing)

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix = maxPointsOfRows `intersect` minPointsOfCols 
    where   matrixList = assocs matrix
            (_, (rows, cols)) = bounds matrix
            maxPointsOfRow i = fmap fst $ filter ((==) maxValue.snd) currentRow
                where currentRow = filter (\((r, _), _) -> r == i) matrixList
                      maxValue   = snd $ maximumBy (comparing snd) currentRow
            minPointsOfCol i = fmap fst $ filter ((==) minValue.snd) currentCol
                where currentCol = filter (\((_, c), _) -> c == i) matrixList
                      minValue   = snd $ minimumBy (comparing snd) currentCol
            maxPointsOfRows = concatMap maxPointsOfRow [0..rows]
            minPointsOfCols = concatMap minPointsOfCol [0..cols]
