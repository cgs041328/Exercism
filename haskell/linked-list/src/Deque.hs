module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad (void)
import Control.Monad.STM (STM, atomically, orElse)
import Control.Concurrent.STM.TMVar
import Control.Applicative ((<$>), (<*>))

data DNode a = DNode { value :: a, prev :: TMVar (DNode a), next :: TMVar (DNode a) }
type Deque a = TMVar (Maybe (DNode a, DNode a))

mkDeque :: IO (Deque a)
mkDeque = atomically $ newTMVar Nothing

emptyNode :: a -> STM (DNode a)
emptyNode x = DNode x <$> newEmptyTMVar <*> newEmptyTMVar

singleton :: Deque a -> DNode a -> STM ()
singleton deq node = putTMVar deq $ Just (node, node)

connect :: DNode a -> DNode a -> STM ()
connect x y = do
    putTMVar (next x) y
    putTMVar (prev y) x

withDeque :: Deque a -> STM () -> STM ()
withDeque deq m = m `orElse` putTMVar deq Nothing

push :: Deque a -> a -> IO ()
push deq x = atomically $ do
    newTail <- emptyNode x
    maybeQ <- takeTMVar deq
    case maybeQ of
        Nothing -> singleton deq newTail
        Just (head, tail) -> do
            tail `connect` newTail
            putTMVar deq $ Just (head, newTail)

pop :: Deque a -> IO (Maybe a)
pop deq = atomically $ runMaybeT $ do
    (head, tail) <- MaybeT $ takeTMVar deq
    lift $ withDeque deq $ do
        newTail <- takeTMVar $ prev tail
        putTMVar deq $ Just (head, newTail)
        void $ takeTMVar $ next newTail
    return $ value tail

unshift :: Deque a -> a -> IO ()
unshift deq x = atomically $ do 
    newHead <- emptyNode x
    maybeQ <- takeTMVar deq
    case maybeQ of
        Nothing -> singleton deq newHead
        Just (head, tail) -> do
            newHead `connect` head
            putTMVar deq $ Just (newHead, tail)

shift :: Deque a -> IO (Maybe a)
shift deq = atomically $ runMaybeT $ do
    (head, tail) <- MaybeT $ takeTMVar deq
    lift $ withDeque deq $ do 
        newHead <- takeTMVar $ next head
        putTMVar deq $ Just (newHead, tail)
        void $ takeTMVar $ prev newHead
    return $ value head
