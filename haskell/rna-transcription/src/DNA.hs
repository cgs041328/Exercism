module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = sequence.fmap transformNucleotide

transformNucleotide :: Char -> Either Char Char
transformNucleotide c
    | c == 'G' = Right 'C'
    | c == 'C' = Right 'G'
    | c == 'T' = Right 'A'
    | c == 'A' = Right 'U'
    |otherwise = Left c