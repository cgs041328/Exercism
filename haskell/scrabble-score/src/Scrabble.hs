module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)
import qualified Data.Map as Map    

letterScoreMap :: Map.Map Char Integer
letterScoreMap = Map.fromList $ concatMap (\(score, word) -> fmap (\letter -> (letter, score)) word) [
    (1, "AEIOULNRST"),
    (2, "DG"),
    (3, "BCMP"),
    (4, "FHVWY"),
    (5, "K"),
    (8, "JX"),
    (10, "QZ")]

scoreLetter :: Char -> Integer
scoreLetter letter = Map.findWithDefault 0 (toUpper letter) letterScoreMap

scoreWord :: String -> Integer
scoreWord word = sum $ fmap scoreLetter word
