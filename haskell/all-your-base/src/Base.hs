module Base (Error(..), rebase) where


data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2  = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | otherwise      =  fmap (fromBaseTen outputBase []) $ toBaseTen inputBase inputDigits

toBaseTen :: Integral a => a -> [a] -> Either (Error a) a
toBaseTen b ds  = fmap sum.sequenceA $ map (\(d, p) -> if d >= b || d < 0 then Left $ InvalidDigit d else Right (d * b ^ p)) $ zip (reverse ds) [0..]

fromBaseTen :: Integral a => a -> [a] -> a -> [a]
fromBaseTen _ digits 0 = digits
fromBaseTen b digits s = fromBaseTen b (m:digits) d
      where (d, m) = s `divMod` b 