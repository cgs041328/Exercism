module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Maybe (mapMaybe)
import Data.Time.Calendar (Day, toGregorian, fromGregorianValid, toModifiedJulianDay)


data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving(Enum, Eq)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth
              

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = pick $ filter (\day -> dayToWeekDay day == weekday) (daysInMonth year month)
  where
    pick = case schedule of
        First  -> (!! 0)
        Second -> (!! 1)
        Third  -> (!! 2)
        Fourth -> (!! 3)
        Last   -> last
        Teenth -> head . filter (\d -> let (_, _, day) = toGregorian d in day `elem` [13..19])

daysInMonth :: Integer -> Int -> [Day]
daysInMonth year month = mapMaybe (fromGregorianValid year month) [1..31]

dayToWeekDay :: Day -> Weekday
dayToWeekDay d = toEnum (fromInteger (toModifiedJulianDay d + 2) `rem` 7)
