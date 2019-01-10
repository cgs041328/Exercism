module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, arbitrary, choose)
import Data.List(sort)

data Character = Character
  { name         :: String
  , strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier n = (n - 10) `div` 2

ability :: Gen Int
ability = do
  first <- choose (1, 6)
  second <- choose (1, 6)
  third <- choose (1, 6)
  forth <- choose (1, 6)
  return $ sum $ tail $ sort [first, second, third, forth]

character :: Gen Character
character = do
  name'         <- arbitrary
  strength'     <- ability
  dexterity'    <- ability
  constitution' <- ability
  intelligence' <- ability
  wisdom'       <- ability
  charisma'     <- ability
  let hitpoints' = modifier constitution' + 10
  return $ Character name' strength' dexterity' constitution' intelligence' wisdom' charisma' hitpoints'

