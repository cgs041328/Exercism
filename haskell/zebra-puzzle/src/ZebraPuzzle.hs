module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Control.Monad (guard)
import Data.List (elemIndex, permutations, zip5)
import Data.Maybe (fromJust)
import Data.Tuple.Curry (uncurryN)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show, Enum, Bounded)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show, Enum, Bounded)

data Beverage = Coffee | Tea | Milk | Juice | Water
  deriving (Eq, Show, Enum, Bounded)

data Cigarette = OldGold | Kools | Chesterfield | LuckyStrike | Parliaments
  deriving (Eq, Show, Enum, Bounded)

data House = House { resident :: Resident
                   , color :: Color
                   , pet   :: Pet
                   , beverage :: Beverage
                   , cigarette :: Cigarette
                   } deriving (Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

solve :: Solution
-- solve = error ""
solve = Solution waterDrinker zebraOwner
  where waterDrinker = resident. head $ filter (\x -> beverage x == Water) solution
        zebraOwner   = resident. head $ filter (\x -> pet x == Zebra) solution

perms :: (Bounded a, Enum a) => [[a]]
perms = permutations [minBound..maxBound]

elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' x xs = fromJust $ elemIndex x xs

solution :: [House]
solution = head $ do
  residents <- perms
  guard (head residents == Norwegian) -- # 10
  colors <- perms
  guard (elemIndex' Englishman residents == elemIndex' Red colors) -- # 2
  guard (elemIndex' Green colors == elemIndex' Ivory colors + 1) -- # 6
  guard (abs (elemIndex' Norwegian residents - elemIndex' Blue colors) == 1) -- # 15
  pets <- perms
  guard (elemIndex' Spaniard residents == elemIndex' Dog pets) -- # 3
  beverages <- perms
  guard (elemIndex' Coffee beverages == elemIndex' Green colors) -- # 4
  guard (elemIndex' Ukrainian residents == elemIndex' Tea beverages) -- # 5
  guard (elemIndex' Milk beverages == 2) -- # 9
  cigarettes <- perms
  guard (elemIndex' OldGold cigarettes == elemIndex' Snails pets) -- # 7
  guard (elemIndex' Kools cigarettes == elemIndex' Yellow colors) -- # 8
  guard (abs (elemIndex' Chesterfield cigarettes - elemIndex' Fox pets) == 1) -- # 11
  guard (abs (elemIndex' Kools cigarettes - elemIndex' Horse pets) == 1) -- # 12
  guard (elemIndex' LuckyStrike cigarettes == elemIndex' Juice beverages) -- # 13
  guard (elemIndex' Parliaments cigarettes == elemIndex' Japanese residents) -- # 14
  return $ uncurryN House <$> zip5 residents colors pets beverages cigarettes
