module Beer (song) where

lync :: Int -> String
lync n 
    | n > 1 = s ++ " of beer on the wall, " ++ s ++" of beer.\n\
                    \Take one down and pass it around, "++ s' ++" of beer on the wall.\n\
                    \\n"
    | n == 1 = "1 bottle of beer on the wall, 1 bottle of beer.\n\
                \Take it down and pass it around, no more bottles of beer on the wall.\n\
                \\n"
    | otherwise = "No more bottles of beer on the wall, no more bottles of beer.\n\
                   \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
                where s  = show' n
                      s' = show' (n - 1)
                      show' x 
                        | x > 1    = show x ++ " bottles"
                        |otherwise = show x ++ " bottle"
song :: String
song = concatMap lync $ reverse [0..99]
