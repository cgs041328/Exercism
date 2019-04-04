module ResistorColors (Color(..), value) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Read, Enum)

value :: [Color] -> Int
value = sum. zipWith (*) powers. map fromEnum. reverse
  where powers = iterate (*10) 1