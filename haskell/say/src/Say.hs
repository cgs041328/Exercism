module Say (inEnglish) where

import Data.Map (Map, (!), fromAscList)
import Data.List(zipWith)

inEnglish :: Integer -> Maybe String
inEnglish n 
    | n < 0     = Nothing
    | n == 0    = Just "zero"
    | otherwise = Just. unwords. reverse. filter (not. null). concat . zipWith (\x y -> if(null y) then [] else x: reverse y) names. fmap upto999 $ repMod 1000 n

units :: Map Integer String
units = fromAscList . zip [1..19] $
    ["one","two","three","four","five","six","seven","eight","nine","ten",
    "eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]

tens :: Map Integer String
tens = fromAscList . zip [2..9] $ ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

names :: [String]
names = ["","thousand","million","billion"]

upto999 :: Integer ->  [String]
upto999 x
    | x == 0                  = []
    | x < 20                  = [units ! x]
    | x <= 99 && head ds == 0 = [tens ! (ds !! 1)]
    | x <= 99                 = [tens ! (ds !! 1) ++ "-" ++ units ! head ds]
    | x <= 999                = [units ! (ds !! 2) , "hundred"] ++ upto999 (x `mod` 100)
        where
            ds = repMod 10 x

repMod :: Integer -> Integer -> [Integer]
repMod _ 0 = []
repMod a b = b `mod` a : repMod a (b `div` a)
