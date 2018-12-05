module ProteinTranslation(proteins) where

import Data.Maybe (isJust)

proteins :: String -> Maybe [String]
proteins = sequence. takeWhile isJust . map translate. splitEvery 3

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

translate :: String -> Maybe String
translate c
  | c == "AUG"                            = Just "Methionine"
  | c `elem` ["UUU", "UUC"]               = Just "Phenylalanine"
  | c `elem` ["UUA", "UUG"]               = Just "Leucine"
  | c `elem` ["UCU", "UCC", "UCA", "UCG"] = Just "Serine"
  | c `elem` ["UAU", "UAC"]               = Just "Tyrosine"
  | c `elem` ["UGU", "UGC"]               = Just "Cysteine"
  | c == "UGG"                            = Just "Tryptophan"
  | otherwise                             = Nothing