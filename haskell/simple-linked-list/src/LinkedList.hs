module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Nil | Cons a (LinkedList a) deriving (Eq, Show)

datum :: LinkedList a -> a
datum (Cons a _) = a

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x:xs) = Cons x $ fromList xs

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = Cons x linkedList

next :: LinkedList a -> LinkedList a
next (Cons a nxt) = nxt

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList l = rev l Nil
        where rev Nil a = a
              rev (Cons x nxt) a = rev nxt (Cons x a)

toList :: LinkedList a -> [a]
toList Nil = []
toList (Cons a nxt) = a : (toList nxt)
