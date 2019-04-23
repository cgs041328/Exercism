module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List ((!!), nub, mapAccumL, mapAccumR)
import Data.Maybe (catMaybes)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)
data Edge = Edge
data Disputed = Disputed deriving (Eq)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = foldl (addIfNew board) [] coords
    where
        coords =  [(x,y) | y <- [1..(length board)], x <- [1..(length $ head board)]]

addIfNew :: [String] -> [(Set Coord, Maybe Color)] -> Coord -> [(Set Coord, Maybe Color)]
addIfNew board acc coord
    | notInTerritories = maybe [] (:[]) (territoryFor board coord) ++ acc
    where
        notInTerritories = all (notElem coord . fst) acc
addIfNew _ acc _ = acc

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord
    | outOfBounds board coord = Nothing
    | empty                   = Nothing
    | otherwise               = Just territory
    where
        territory = unwrapDisputes $ determineTerritory board coord Set.empty
        empty     = null $ fst territory

outOfBounds :: [String] -> Coord -> Bool
outOfBounds board (x,y)
  | y <= 0                  = True
  | x <= 0                  = True
  | y > length board        = True
  | x > length (board !! 0) = True
  | otherwise               = False

unwrapDisputes :: (Set Coord, Either Disputed (Maybe Color)) -> (Set Coord, Maybe Color)
unwrapDisputes (coords, Left _)      = (coords, Nothing)
unwrapDisputes (coords, Right owner) = (coords, owner)

determineTerritory :: [String] -> Coord -> Set Coord -> (Set Coord, Either Disputed (Maybe Color))
determineTerritory board coord territory
    | alreadyVisited = (territory, Right Nothing)     -- Already visited spots make no ownership claim
    | otherwise      = case (statusOf board coord) of
        Right (Just color) -> (territory, Right $ Just color)
        Right Nothing      -> expand board (neighborsOf coord) $ Set.insert coord territory
        otherwise          -> (territory, Right Nothing)       -- Edges make no ownership claim
    where
        alreadyVisited    = Set.member coord territory
        neighborsOf (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

statusOf :: [String] -> Coord -> Either Edge (Maybe Color)
statusOf board (x,y)
    | outOfBounds board (x,y) = Left Edge
    | value == 'B'            = Right $ Just Black
    | value == 'W'            = Right $ Just White
    | otherwise               = Right Nothing
    where
        value = (board !! (y-1)) !! (x-1)

expand :: [String] -> [Coord] -> Set Coord -> (Set Coord, Either Disputed (Maybe Color))
expand board neighbors currentTerritory = (expandedTerritory, foldl1 determineControl owners )
    where
        neighborsTerritory          = flip (determineTerritory board)
        (expandedTerritory, owners) = mapAccumL neighborsTerritory currentTerritory neighbors

determineControl :: Either Disputed (Maybe Color) -> Either Disputed (Maybe Color) -> Either Disputed (Maybe Color)
determineControl current new | current == new  = current
determineControl (Right Nothing) new           = new
determineControl current (Right Nothing)       = current
determineControl _ _                           = Left Disputed