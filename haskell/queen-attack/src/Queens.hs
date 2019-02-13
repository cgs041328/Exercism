module Queens (boardString, canAttack) where

import Data.List (intersperse)
import Data.List.Split (chunksOf)

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ fmap (intersperse ' '. fmap toPiece) $ chunksOf 8 [0..63]
    where toPiece i
            | Just i == whitePosition = 'W'
            | Just i == blackPosition = 'B'
            | otherwise               = '_'
          whitePosition = fmap toPosition white
          blackPosition = fmap toPosition black
          toPosition (x, y) = y + x * 8

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (aX, aY) (bX, bY) = aX == bX || aY == bY || abs(aX - bX) == abs(aY - bY)
