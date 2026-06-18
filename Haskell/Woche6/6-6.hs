import Data.List

class Sortable a where
    sortIncr :: a -> a
    sortDecr :: a -> a
    
class Sortable a => UniqueSorted a where
    usortIncr :: a -> a
    usortDecr :: a -> a
    
data HTree a b = HNil |
                Node1 a (HTree a b) (HTree a b) |
                Node2 a b (HTree a b) (HTree a b) deriving Show

data Label a b = L1 a | L2 a b deriving Eq

instance (Ord a, Eq b) => Ord (Label a b) where
    compare (L1 a1)   (L1 a2)   = compare a1 a2
    compare (L1 a1)   (L2 a2 _) = compare a1 a2
    compare (L2 a1 _) (L1 a2)   = compare a1 a2
    compare (L2 a1 _) (L2 a2 _) = compare a1 a2

reverseList :: Ord a => [Label a b] -> [Label a b]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

treeToList :: Ord a => HTree a b -> [Label a b]
treeToList HNil = []
treeToList (Node1 root left right) = L1 root:(treeToList left) ++ (treeToList right)
treeToList (Node2 root1 root2 left right) = (L2 root1 root2):(treeToList left) ++ (treeToList right)

listToTree :: Ord a => [Label a b] -> HTree a b
listToTree [] = HNil
listToTree ((L1 x):xs) = Node1 x HNil (listToTree xs)
listToTree ((L2 x y):xs) = Node2 x y HNil (listToTree xs)

instance (Ord a, Eq b) => Sortable (HTree a b) where
    sortIncr HNil = HNil
    sortIncr t = listToTree (sort (treeToList t))
    sortDecr HNil = HNil
    sortDecr t = listToTree (reverseList (sort (treeToList t)))
    
instance (Ord a, Eq b) => UniqueSorted (HTree a b) where
    usortIncr HNil = HNil
    usortIncr t = listToTree (sort (nub(treeToList t)))
    usortDecr HNil = HNil
    usortDecr t = listToTree (reverseList (sort (nub (treeToList t))))