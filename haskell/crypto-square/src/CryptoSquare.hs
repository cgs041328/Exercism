module CryptoSquare (encode) where

import Data.Char(isAlphaNum, toLower)
import Data.List.Split(chunksOf)
import Data.List(transpose, intercalate)

encode :: String -> String
encode xs = intercalate " ". transpose. chunksOf c. fmap toLower $ filter isAlphaNum xs
    where c = (floor. sqrt . fromIntegral. length) xs