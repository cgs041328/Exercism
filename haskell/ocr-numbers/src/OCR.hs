module OCR (convert) where

import Data.List (transpose, intercalate)
import Data.List.Split(chunksOf)
import qualified Data.Map.Strict as M

allFonts :: [String]
allFonts = [  " _     _  _     _  _  _  _  _ "
            , "| |  | _| _||_||_ |_   ||_||_|"
            , "|_|  ||_  _|  | _||_|  ||_| _|"
            , "                              "
           ]

fontMap :: M.Map [String] Char
fontMap = M.fromList $ splitRow allFonts `zip` ['0'..]

splitRow :: [String] -> [[String]]
splitRow = transpose. fmap (chunksOf 3)

recognizeRow :: [String] -> String          
recognizeRow = fmap recognize. splitRow
    where recognize x = M.findWithDefault '?' x fontMap 

convert :: String -> String
convert = intercalate ",". fmap recognizeRow. chunksOf 4. lines

