module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Nil | Node (BST a) a (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Nil          = Nothing
bstLeft (Node l _ _) = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight Nil          = Nothing
bstRight (Node _ _ r) = Just r

bstValue :: BST a -> Maybe a
bstValue Nil          = Nothing
bstValue (Node _ v _) = Just v

empty :: BST a
empty = Nil

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x Nil = singleton x
insert x (Node l v r) = if x <= v then Node (insert x l) v r else Node l v (insert x r)

singleton :: a -> BST a
singleton x = Node Nil x Nil

toList :: BST a -> [a]
toList Nil           = []
toList (Node l v r)  = toList l ++ (v : toList r)
