module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock {
    hour    :: Int
  , minute  :: Int
} deriving (Eq) 

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = fromMin (h * 60 + m)

toString :: Clock -> String
toString clock = (show'. hour) clock ++ ":" ++ (show'. minute) clock
    where show' = padLeft 2 '0'. show

padLeft :: Int -> Char -> String -> String
padLeft n c xs = replicate (n - length xs) c ++ xs

addDelta :: Int -> Int -> Clock -> Clock
addDelta h m clock = fromMin $ toMin clock + h * 60 + m

fromMin :: Int -> Clock
fromMin n = Clock h m
    where m = n `mod` 60
          h = (n `div` 60) `mod` 24

toMin :: Clock -> Int
toMin (Clock h m) = h * 60 + m
