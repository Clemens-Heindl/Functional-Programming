import Control.Monad.State
import Data.List
import Data.Maybe

data TripleTree a = Leaf a | Node a (TripleTree a) (TripleTree a) (TripleTree a) deriving (Eq, Show)

type RenameState = State (Int)

numberTree :: (Eq a, Show a) => TripleTree a -> TripleTree Int
numberTree t = evalState (renameTreeMonad t) (1)

renameTreeMonad :: Eq a => TripleTree a -> RenameState (TripleTree Int)
renameTreeMonad (Leaf x) = do
    next <- get
    put (next+1)
    return (Leaf next)

renameTreeMonad (Node root left mid right) = do
    left' <- renameTreeMonad left
    mid' <- renameTreeMonad mid
    right' <- renameTreeMonad right
    next <- get
    put (next+1)
    return (Node next left' mid' right')