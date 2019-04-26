{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , empty
  ) where

import Control.Applicative hiding (empty)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.Char
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Control.Monad
import Data.Attoparsec.Text as A

type Op = Text
type Action = ForthState -> Either ForthError ForthState

data ForthState = ForthState [Int] (Map Op Action)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

empty :: ForthState
empty = ForthState [] (Map.fromList
                   [ ("over", uncheckedBinList (\x y -> [x,y,x]))
                   , ("swap", uncheckedBinList (\x y -> [x,y]))
		   , ("drop", uncheckedUniList (const []))
		   , ("dup", uncheckedUniList (\x -> [x,x]))
		   , ("/", divi)
		   , ("*", uncheckedBin1 (*))
		   , ("-", uncheckedBin1 (-))
		   , ("+", uncheckedBin1 (+))
		   ])

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText input st = 
  case parseOnly forth input of
    Right r -> foldM process st r
    Left s  -> error s

toList :: ForthState -> [Int]
toList (ForthState stack _) = reverse stack

process :: ForthState -> Dictionary -> Either ForthError ForthState
process s@(ForthState ls ops) (N v) =
   return $ ForthState (v:ls) ops 
process s@(ForthState ls ops) (W w) =
    case Text.toLower w `Map.lookup` ops of
      Just op -> op s
      Nothing -> Left (UnknownWord w)
process s@(ForthState ls ops) (Defn t act)
    | isDigit (Text.head t) = Left $ InvalidWord -- XXX move to parser
    | otherwise = return $ ForthState ls (Map.insert (Text.toLower t) (flip (foldM process) act) ops)

uncheckedBin1 :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
uncheckedBin1 f (ForthState (x0:x1:xs) ops) = return $ ForthState (f x1 x0 :xs) ops
uncheckedBin1 _ _ = Left StackUnderflow

divi :: Action
divi (ForthState (0:x1:xs) ops)  = Left DivisionByZero
divi (ForthState (x0:x1:xs) ops) = return $ ForthState (x1 `div` x0:xs) ops
divi _ = Left StackUnderflow

uncheckedUniList :: (Int -> [Int]) -> ForthState -> Either ForthError ForthState
uncheckedUniList f (ForthState (x:xs) ops) = return $ ForthState (f x ++ xs) ops
uncheckedUniList _ _ = Left StackUnderflow

uncheckedBinList :: (Int -> Int -> [Int]) -> ForthState -> Either ForthError ForthState
uncheckedBinList f (ForthState (x0:x1:xs) ops) = return $ ForthState (f x1 x0 ++ xs) ops
uncheckedBinList _ _ = Left StackUnderflow

data Dictionary = Defn Text [Dictionary]
                | N Int
		| W Text
		deriving (Eq, Show)

forth :: Parser [Dictionary]
forth = (defn <|> f1) `sepBy` anyChar

f1 :: Parser Dictionary
f1 = choice [N <$> decimal, W <$> A.takeWhile1 (/=' ')]

defn :: Parser Dictionary
defn = Defn <$> (string ": " *> A.takeWhile1 (/=' ')) <*> manyTill (skipSpace *> f1) (skipSpace *> char ';') <?> "definition"