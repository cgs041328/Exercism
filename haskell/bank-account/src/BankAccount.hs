module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.MVar
import Control.Arrow ((&&&))

type BankAccount = MVar (Maybe Balance) 
type Balance = Integer

closeAccount :: BankAccount -> IO ()
closeAccount = flip modifyMVar_ (const $ return Nothing)

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = readMVar

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = modifyMVar account (return. (id &&& id). fmap ( + amount))

openAccount :: IO BankAccount
openAccount = newMVar (Just 0)
