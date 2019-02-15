module Frequency (frequency) where

import qualified Control.Parallel.Strategies as S
import Data.Char ( isLetter, toLower )
import qualified Data.Map.Strict as M
import qualified Data.Text as T

frequency :: Int -> [T.Text] -> M.Map Char Int
frequency n = M.unionsWith (+)
            . S.withStrategy (S.parBuffer n S.rdeepseq)
            . map frequency'

frequency' :: T.Text -> M.Map Char Int
frequency' = T.foldl' f M.empty . T.filter isLetter
  where f counts char = M.insertWith (+) (toLower char) 1 counts
