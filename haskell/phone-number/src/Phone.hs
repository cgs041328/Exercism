module Phone (number) where

import Data.Char (isDigit)
import Control.Monad ((>=>))

otherValidChars :: String
otherValidChars = " -+.()"

isValidChar :: Char -> Bool
isValidChar c = isDigit c || elem c otherValidChars

validateChars :: String -> Maybe String
validateChars phone
  | not $ all isValidChar phone = Nothing
  | otherwise = Just $ filter isDigit phone

validateEleven :: String -> Maybe String
validateEleven phone
  | head phone == '1' = validateAreaCode $ drop 1 phone
  | otherwise = Nothing

validateLength :: String -> Maybe String
validateLength phone
  | len < 10 || len > 11 = Nothing
  | len == 11 = validateEleven phone
  | otherwise = validateAreaCode phone
  where len = length phone

validateAreaCode :: String -> Maybe String
validateAreaCode [] = Nothing
validateAreaCode xs  
    | head xs == '0' || head xs == '1' || (head . drop 3) xs == '0' || (head . drop 3) xs == '1' = Nothing
    | otherwise                        = Just xs

number :: String -> Maybe String
number = validateChars >=> validateLength
