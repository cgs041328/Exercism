module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import qualified Data.Map.Strict as M
import Data.List (transpose)
import Data.List.Split (chunksOf)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Garden = M.Map String [Plant]

garden :: [String] -> String -> Garden
garden students plants = M.fromList .zip students. fmap (fmap plantMap. concat). transpose. fmap (chunksOf 2) $ lines plants
    where 
        plantMap p = case p of
            'R' -> Radishes
            'C' -> Clover
            'G' -> Grass
            'V' -> Violets
            _ -> error $ show p ++ " is not a valid plant"

lookupPlants :: String -> Garden -> [Plant]
lookupPlants = M.findWithDefault []
