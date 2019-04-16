module Poker (bestHands) where

import Data.Function (on)
import Data.List (group, sort, maximumBy, sortBy)
import Data.Ord (comparing)

data Suit
    = Diamonds
    | Clubs
    | Hearts
    | Spades
    deriving (Eq, Show)

data Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    deriving (Eq, Show, Ord, Enum)

data Card = Card
    { cardRank :: Rank
    , cardSuit :: Suit
    } deriving (Eq, Show)

data Category
    = HighCard Hand
    | OnePair Rank Rank Rank Rank
    | TwoPairs Rank Rank Rank
    | ThreeOfAKind Rank Rank Rank
    | Straight Rank
    | Flush Hand
    | FullHouse Rank Rank
    | FourOfAKind Rank Rank
    | StraightFlush Rank
    deriving (Eq, Show, Ord)

type Hand = [Card]

instance Ord Card where
    compare = compare `on` cardRank

ranks :: [(String, Rank)]
ranks =
    [ ("2", Two)
    , ("3", Three)
    , ("4", Four)
    , ("5", Five)
    , ("6", Six)
    , ("7", Seven)
    , ("8", Eight)
    , ("9", Nine)
    , ("10", Ten)
    , ("J", Jack)
    , ("Q", Queen)
    , ("K", King)
    , ("A", Ace)
    ]

suits :: [(Char, Suit)]
suits =
    [ ('D', Diamonds)
    , ('C', Clubs)
    , ('H', Hearts)
    , ('S', Spades)
    ]

bestHands :: [String] -> Maybe [String]
bestHands xs = do
    hands <- mapM parseHand xs
    let best = maximaBy (comparing category) hands
    return (map printHand best)

parseHand :: String -> Maybe Hand
parseHand = mapM parseCard . words
  where
    parseCard [] = Nothing
    parseCard xs = Card <$> parseRank (init xs) <*> parseSuit (last xs)
    parseRank x = lookup x ranks
    parseSuit x = lookup x suits

printHand :: Hand -> String
printHand = unwords . map printCard
  where
    printCard (Card rank suit) = printRank rank ++ [printSuit suit]
    printRank rank = head [s | (s, r) <- ranks, r == rank]
    printSuit suit = head [c | (c, s) <- suits, s == suit]

category :: Hand -> Category
category hand
    | straightHi && flush = StraightFlush (last sortedRanks)
    | straightLo && flush = StraightFlush (maximum (init sortedRanks))
    | flush               = Flush (sortBy (flip (comparing cardRank)) hand)
    | straightHi          = Straight (last sortedRanks)
    | straightLo          = Straight (maximum (init sortedRanks))
    | otherwise           = case sortBy (comparing length) (group sortedRanks) of
        [[g],[h,_,_,_]]     -> FourOfAKind h g
        [[g,_],[h,_,_]]     -> FullHouse h g
        [[g],[h],[i,_,_]]   -> ThreeOfAKind i h g
        [[g],[h,_],[i,_]]   -> TwoPairs i h g
        [[g],[h],[i],[j,_]] -> OnePair j i h g
        _                   -> HighCard (sortBy (flip (comparing cardRank)) hand)
  where
    flush = allEqual (map cardSuit hand)
    straightHi = ascending sortedRanks
    straightLo = ascending (init sortedRanks) && last sortedRanks == Ace
    sortedRanks = sort (map cardRank hand)

ascending :: (Enum a, Eq a) => [a] -> Bool
ascending xs = all (\(x,y) -> succ x == y) (zip xs (tail xs))

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

maximaBy :: Eq a => (a -> a -> Ordering) -> [a] -> [a]
maximaBy cmp xs = filter (\x -> cmp x (maximumBy cmp xs) == EQ) xs
