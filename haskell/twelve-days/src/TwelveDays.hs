module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = take (stop - start + 1) $ drop (start - 1) lyncs

lyncs :: [String]
lyncs = map (\(x, y) -> x ++ ": " ++ y ++ ".") $ zip [firstHalf n | n <- [1..12]] secondHalf

firstHalf :: Int -> String
firstHalf n = "On the " ++ nth ++ " day of Christmas my true love gave to me"
    where nth = [ "first"
                , "second"
                , "third"
                , "fourth"
                , "fifth"
                , "sixth"
                , "seventh"
                , "eighth"
                , "ninth"
                , "tenth"
                , "eleventh"
                , "twelfth"] !! (n-1)

secondHalf :: [String]
secondHalf  =  scanl (\acc (b, n) -> let acc' = if n == 2 then "and " ++ acc else acc in b ++ ", " ++ acc') (presents !! 0)
    $ zip (drop 1 presents) ([2..12] :: [Integer])

presents :: [String]
presents = [ "a Partridge in a Pear Tree"
           , "two Turtle Doves"
           , "three French Hens"
           , "four Calling Birds"
           , "five Gold Rings"
           , "six Geese-a-Laying"
           , "seven Swans-a-Swimming"
           , "eight Maids-a-Milking"
           , "nine Ladies Dancing"
           , "ten Lords-a-Leaping"
           , "eleven Pipers Piping"
           , "twelve Drummers Drumming"]

