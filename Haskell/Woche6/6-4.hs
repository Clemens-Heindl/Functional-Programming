import Data.List

class Sortable a where
    sortIncr :: a -> a
    sortDecr :: a -> a
    
data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show


reverseList :: Ord a => [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

treeToList :: Ord a => Tree a -> [a]
treeToList Nil = []
treeToList (Node root left right) = root:(treeToList left) ++ (treeToList right)

listToTree :: Ord a => [a] -> Tree a
listToTree [] = Nil
listToTree (x:xs) = Node x Nil (listToTree xs)

instance Ord a => Sortable [a] where
    sortIncr [] = []
    sortIncr l = sort l
    sortDecr [] = []
    sortDecr l = reverseList (sort l)
    
instance Ord a => Sortable (Tree a) where
    sortIncr Nil = Nil
    sortIncr t = listToTree (sort (treeToList t))
    sortDecr Nil = Nil
    sortDecr t = listToTree (reverseList (sort (treeToList t)))