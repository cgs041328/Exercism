{-# LANGUAGE TemplateHaskell #-}

module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  ) where

import Data.Time.Calendar
import Control.Lens

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     }

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 }             

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 }               

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       }

data Gregorian = Gregorian {
      _year  :: Integer,
      _month :: Int,
      _day   :: Int
    }

makeLenses ''Gregorian
makeLenses ''Person
makeLenses ''Name
makeLenses ''Born     
makeLenses ''Address         

dayGregorian :: Iso' Day Gregorian
dayGregorian = iso ((\(y, m, d) -> Gregorian y m d) . toGregorian)
                   (\(Gregorian y m d) -> fromGregorian y m d)

bornStreet :: Born -> String
bornStreet = (^. bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = (address . street .~)

setBirthMonth :: Int -> Person -> Person
setBirthMonth = (born . bornOn . dayGregorian . month .~)

renameStreets :: (String -> String) -> Person -> Person
renameStreets f = (born.bornAt.street %~ f) . (address.street %~ f)
