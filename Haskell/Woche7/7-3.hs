import Control.Monad.State
import Data.List
import Data.Maybe

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Eq, Show)

type RenameState a = State ([a], Int)

renameTree :: (Eq a, Show a) => Tree a -> Tree Int
renameTree t = evalState (renameTreeMonad t) ([], 1)

renameTreeMonad :: Eq a => Tree a -> RenameState a (Tree Int)
renameTreeMonad (Leaf x) = do
    (visited, next) <- get
    if x `elem` visited
       then return (Leaf((fromJust (elemIndex x visited))+1))
       else do
           put (visited ++ [x], next + 1)
           return (Leaf next)
renameTreeMonad (Node root left right) = do
    (visited, next) <- get
    if root `elem` visited
       then do
          left' <- renameTreeMonad left
          right' <- renameTreeMonad right
          return (Node ((fromJust (elemIndex root visited))+1) left' right')
       else do
           put (visited ++ [root], next + 1)
           left' <- renameTreeMonad left
           right' <- renameTreeMonad right
           return (Node next left' right')