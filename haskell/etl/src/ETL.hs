module ETL (transform) where

import Data.Map.Strict (Map, foldrWithKey, empty, insert)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = foldrWithKey (\k s m -> foldr (\c m' -> insertNew k c m') m s) empty legacyData
    where insertNew n c m = insert (toLower c) n m 
