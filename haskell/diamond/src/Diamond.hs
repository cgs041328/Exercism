module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond char = Just $ [rowFor letter | letter <- verticalPattern]
  where
    -- ABCD
    letters = ['A' .. char]
    -- DCBABCD
    horizontalPattern = (reverse . drop 1 $ letters) ++ letters
    -- ABCDCBA
    verticalPattern = letters ++ (drop 1 . reverse $ letters)
    -- rowFor 'A' -> "···A···", rowFor 'B' -> "··B·B··"...
    rowFor :: Char -> String
    rowFor letter = map mask horizontalPattern
      where
        mask :: Char -> Char
        mask c
          | c == letter = letter
          | otherwise = ' '
