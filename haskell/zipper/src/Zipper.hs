module Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 ) where

import Data.Maybe        (fromJust)

data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

data Crumb a = LeftCrumb a (Maybe (BinTree a)) | RightCrumb a (Maybe (BinTree a)) deriving (Eq, Show)

type Breadcrumbs a = [Crumb a]  

type Zipper a = (BinTree a, Breadcrumbs a)

fromTree :: BinTree a -> Zipper a
fromTree tree = (tree, [])

toTree :: Zipper a -> BinTree a
toTree = fst. topMost

topMost :: Zipper a -> Zipper a  
topMost (t,[]) = (t,[])  
topMost z = topMost $ fromJust $ up z  

value :: Zipper a -> a
value = btValue. fst

left :: Zipper a -> Maybe (Zipper a)
left (BT { btValue = v, btLeft = Just l, btRight = r}, bs) = Just (l, LeftCrumb v r:bs)
left _                                                     = Nothing

right :: Zipper a -> Maybe (Zipper a)
right (BT { btValue = v, btLeft = l, btRight = Just r}, bs) = Just (r, RightCrumb v l:bs)
right _                                                     = Nothing

up :: Zipper a -> Maybe (Zipper a)
up (_, [])               = Nothing
up (l, LeftCrumb v r:bs) = Just (BT { btValue = v, btLeft = Just l, btRight = r}, bs)
up (r, RightCrumb v l:bs) = Just (BT { btValue = v, btLeft = l, btRight = Just r}, bs)

setValue :: a -> Zipper a -> Zipper a
setValue x (BT { btValue = v, btLeft = l, btRight = r}, bs) = (BT { btValue = x, btLeft = l, btRight = r}, bs)

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft tree (BT { btValue = v, btLeft = l, btRight = r}, bs) = (BT { btValue = v, btLeft = tree, btRight = r}, bs)

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight tree (BT { btValue = v, btLeft = l, btRight = r}, bs) = (BT { btValue = v, btLeft = l, btRight = tree}, bs)
