module School (School, add, empty, grade, sorted) where

import qualified Data.Map.Strict as Map
import Data.List(sort)
import Data.Maybe(fromMaybe)

type School = Map.Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student school = Map.insertWith (++) gradeNum [student] school

empty :: School
empty = Map.empty

grade :: Int -> School -> [String]
grade gradeNum school = sort $ fromMaybe [] $ Map.lookup gradeNum school 

sorted :: School -> [(Int, [String])]
sorted = map (\(i,xs) -> (i, sort xs)). Map.toAscList
