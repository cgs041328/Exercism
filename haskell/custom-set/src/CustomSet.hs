module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)
import Data.Function (on)

data CustomSet a = Nil | Node Int (CustomSet a) a (CustomSet a) deriving(Show)

instance Ord a => Eq (CustomSet a) where
  (==) = (==) `on` toList
  (/=) = (/=) `on` toList

instance Ord a => Ord (CustomSet a) where
  compare = compare `on` toList

delete :: (Ord a) => a -> CustomSet a -> CustomSet a
delete _ Nil = Nil
delete x n@(Node s l v r)
    | x == v     = deleteRoot n
    | x < v      = Node (s-1) (delete x l) v r
    | otherwise  = Node (s-1) l v (delete x r)

deleteRoot :: (Ord a) => CustomSet a -> CustomSet a
deleteRoot Nil              = Nil 
deleteRoot (Node _ Nil _ r) = r
deleteRoot (Node _ l _ Nil) = l
deleteRoot (Node s l _ r)   = Node (s-1) l v r'
  where 
    v = leftistElement r
    r' = delete v r
    leftistElement Nil               = error "no leftist element."
    leftistElement (Node _ Nil v' _) = v'
    leftistElement (Node _ l' _ _)   = leftistElement l'

difference :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = foldr delete setA (toList setB)

empty :: CustomSet a
empty = Nil

fromList :: (Ord a) => [a] -> CustomSet a
fromList = foldl (flip insert) empty

insert :: (Ord a) => a -> CustomSet a -> CustomSet a
insert x Nil            = Node 1 Nil x Nil
insert x n@(Node _ l v r) = if x < v then let l' = insert x l in Node (size l' + size r + 1) l' v r 
                            else if x > v then let r' = insert x r in Node (size r' + size l + 1) l v r' else n

intersection :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = fromList $ filter (flip member setA) $ toList setB

isDisjointFrom :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = not.any (flip member setB) $ toList setA

isSubsetOf :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = all (flip member setB) $ toList setA

member :: (Ord a) => a -> CustomSet a -> Bool
member _ Nil = False
member x (Node _ l v r)
    | x == v     = True
    | x < v      = member x l
    | otherwise  = member x r

null :: CustomSet a -> Bool
null Nil = True
null _   = False

size :: CustomSet a -> Int
size Nil = 0
size (Node s _ _ _) = s

toList :: CustomSet a -> [a]
toList Nil           = []
toList (Node _ l v r)  = toList l ++ (v : toList r)

union :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union setA setB = foldl (flip insert) setA $ toList setB
