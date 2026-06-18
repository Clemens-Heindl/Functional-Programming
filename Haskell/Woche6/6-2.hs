import Data.List
data Tree a = Nil | Leaf a | Node a (Tree a) (Tree a) deriving (Eq, Show)

reverseList :: Ord a => [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

mirror :: Ord a => Tree a -> Tree a
mirror (Node root left right) = (Node root (mirror right) (mirror left))
mirror c = c

lsortIncr :: Ord a => [a] -> [a]
lsortIncr l = sort l


lsortDecr :: Ord a => [a] -> [a]
lsortDecr l = reverseList (sort l)

treeToList :: Ord a => Tree a -> [a]
treeToList Nil = []
treeToList (Leaf c) = [c]
treeToList (Node root left right) = root:(treeToList left) ++ (treeToList right)

listToTree :: Ord a => [a] -> Tree a
listToTree [] = Nil
listToTree l
    | len == 1 = Leaf (l!!0)
    | otherwise = Node (l!!mid) (listToTree(take mid l)) (listToTree(drop (mid+1) l))
    where
        len = length l
        mid = len `div` 2

tsortIncr :: Ord a => Tree a -> Tree a
tsortIncr Nil = Nil
tsortIncr (Leaf c) = Leaf c
tsortIncr t = listToTree (lsortIncr (treeToList t))

tsortDecr :: Ord a => Tree a -> Tree a
tsortDecr Nil = Nil
tsortDecr (Leaf c) = Leaf c
tsortDecr t = listToTree (lsortDecr (treeToList t))