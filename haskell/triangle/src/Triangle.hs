module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: Num a => a -> a -> a -> TriangleType
triangleType a b c
  | not legal    = Illegal
  | different    = Scalene
  | equal        = Equilateral
  | otherwise    = Isosceles
  where legal     = a + b > c && a + c > b && b + c > a
        equal     = a == b && b == c
        different = a /= b && b /= c && a /= c
