module Alphametics (solve) where

import Data.List
import Data.List.Split
import Data.Map.Strict(fromList, (!))
import Data.Char(intToDigit)

solve :: String -> Maybe [(Char, Int)]
solve puzzle = safeHead $ filter (validate left right) candidates
    where (left, right) = parse puzzle
          allCharaters = nub. concat $ left ++ [right]
          candidates = fmap (zip allCharaters) $ permutations [0..9]
          safeHead []    = Nothing
          safeHead (x:_) = Just x

parse :: String -> ([String], String)
parse sentence = (left, right)
    where 
        sents = splitOn " == " sentence
        left = splitOn " + " $ head sents
        right = last sents

validate :: [String] -> String -> [(Char, Int)] -> Bool
validate left right ms = all notLeadingZero convertedLeft && notLeadingZero convertedRight && sum (fmap read convertedLeft) == read convertedRight
    where dict = fromList ms
          convert = fmap (intToDigit. (dict !))
          convertedLeft = fmap convert left
          convertedRight = convert right
          notLeadingZero s = not $ head s == '0'