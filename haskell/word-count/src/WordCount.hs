module WordCount (wordCount) where

import qualified Data.Map.Strict as M
import Data.Char(toLower, isAlphaNum)
import Data.List.Split(splitWhen)

wordCount :: String -> [(String, Int)]
wordCount = M.toList. foldl (\m w -> M.insertWith (+) w 1 m) M.empty.fmap removeQuotation. (fmap. fmap) toLower.filter (not.null). splitWhen toSplit
    where toSplit s = not (isAlphaNum s || s == '\'')
          removeQuotation s = let o = if head s == '\'' then tail s else s in if last o == '\'' then init o else o
