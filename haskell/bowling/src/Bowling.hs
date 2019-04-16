{-# OPTIONS_GHC -Wall -Werror #-}

module Bowling
  ( score
  , BowlingError(..)
  ) where

import Data.Foldable (foldlM)

data BowlingError
  = IncompleteGame
  | InvalidRoll { rollIndex :: Int
                , rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls =
  foldlM folder ([], 0, 0 :: Int) (zip [0 ..] rolls) >>= verifyCompleteGame
  where
    verifyCompleteGame (_, total, 10) = Right total
    verifyCompleteGame _ = Left IncompleteGame
    folder (queue, _, frame) (xi, x)
      | frame == 10 ||
          x < 0 || x > 10 || tooManyPins 0 || tooManyPins 1 && head queue == 10 =
        Left $ InvalidRoll xi x
      where
        tooManyPins i =
          length queue == i + 1 && queue !! i /= 10 && queue !! i + x > 10
    folder (queue, total, frame) (_, x) =
      Right
        (if length queue == 2 || length queue == 1 && head queue + x < 10
           then ( case queue of
                    [10, y] -> [y, x]
                    [_, _] -> [x]
                    _ -> []
                , total + sum queue + x
                , frame + 1)
           else (queue ++ [x], total, frame))