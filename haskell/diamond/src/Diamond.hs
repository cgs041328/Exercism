module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond char = Just $ [[ mask x y | x <- horizontalPattern] | y <- verticalPattern]
  where
    -- ABCD
    letters = ['A' .. char]
    -- DCBABCD
    horizontalPattern = (reverse . drop 1 $ letters) ++ letters
    -- ABCDCBA
    verticalPattern = letters ++ (drop 1 . reverse $ letters)
    -- rowFor 'A' -> "···A···", rowFor 'B' -> "··B·B··"...
    mask :: Char -> Char -> Char
    mask c c'
      | c == c'   = c
      | otherwise = ' '
