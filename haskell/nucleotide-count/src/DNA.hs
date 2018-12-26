module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, insertWith, fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)


nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldr nucleotideCount (Right initial) xs
    where initial = fromList [(A,0),(C,0),(G,0),(T,0)]

nucleotideCount ::  Char -> Either String (Map Nucleotide Int) -> Either String (Map Nucleotide Int)
nucleotideCount c em = case em of
    Left x -> Left x
    Right m   | c == 'A'  -> Right $ insertWith (+) A 1 m
              | c == 'C'  -> Right $ insertWith (+) C 1 m
              | c == 'G'  -> Right $ insertWith (+) G 1 m
              | c == 'T'  -> Right $ insertWith (+) T 1 m
              | otherwise -> Left "Invalid nucleotide."

