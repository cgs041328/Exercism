module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, insertWith, fromList)
import Control.Monad(foldM)
import Text.Read(readMaybe) 

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read, Enum)

instance Bounded (Nucleotide) where
    minBound = A
    maxBound = T

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldM nucleotideCount initial xs
    where initial = fromList $ [(n,0) | n <- [minBound..maxBound]]

nucleotideCount :: Map Nucleotide Int -> Char -> Either String (Map Nucleotide Int)
nucleotideCount m c = 
    case readMaybe [c] of
        Nothing         -> Left "Invalid nucleotide."
        Just nucleotide -> Right $ insertWith (+) nucleotide 1 m

