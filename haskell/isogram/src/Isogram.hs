module Isogram (isIsogram) where

import qualified Data.Set as Set    
import Data.Char (toLower)
import Control.Monad (liftM2)

isIsogram :: String -> Bool
isIsogram = not. hasDuplicatesWith Set.empty. filter (liftM2 (&&) (/='-') (/=' '))

hasDuplicatesWith :: Set.Set Char -> String -> Bool
hasDuplicatesWith _ []        = False 
hasDuplicatesWith seen (x:xs) = x' `Set.member` seen || hasDuplicatesWith (Set.insert x' seen) xs
        where x' = toLower x