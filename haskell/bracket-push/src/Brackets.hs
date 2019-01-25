module Brackets (arePaired) where

import Data.Stack
import Data.Map.Strict

arePaired :: String -> Bool
arePaired = f stackNew
    where f st [] = stackIsEmpty st
          f st (c:cs) 
            | c `elem` "[{(" = f (stackPush st c) cs
            | c `elem` "]})" = case stackPop st of
                                    Nothing -> False
                                    Just (st', c') -> if bracketMap ! c  == c'  then f st' cs else False
            | otherwise      = f st cs
          bracketMap = fromList [(']','['), ('}','{'), (')', '(')]