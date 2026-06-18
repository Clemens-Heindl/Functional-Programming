import Data.List
data Tree a = Nil | Leaf a | Node a (Tree a) (Tree a) deriving (Eq, Show)

reverseList :: Ord a => [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

mirror :: Ord a => Tree a -> Tree a
mirror (Node root left right) = (Node root (mirror right) (mirror left))
mirror c = c

ulsortIncr :: Ord a => [a] -> [a]
ulsortIncr l = sort (nub l)


ulsortDecr :: Ord a => [a] -> [a]
ulsortDecr l = reverseList (sort (nub l))

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
        
usort :: Ord a => [a] -> [a]
usort [] = []   
usort l
    | (length unique `mod` 2) == 0 = oneDuplicate (sort l)
    | otherwise = sort unique
    where 
        unique = nub l

oneDuplicate :: Eq a => [a] -> [a]
oneDuplicate xs = go xs [] False
  where
    go [] _ _ = []
    go (y:ys) seen used
        | y `notElem` seen = y : go ys (y:seen) used
        | not used = y : go ys seen True
        | otherwise = go ys seen used

utsortIncr :: Ord a => Tree a -> Tree a
utsortIncr Nil = Nil
utsortIncr (Leaf c) = Leaf c
utsortIncr t = listToTree (usort(treeToList t))

utsortDecr :: Ord a => Tree a -> Tree a
utsortDecr Nil = Nil
utsortDecr (Leaf c) = Leaf c
utsortDecr t = listToTree (reverseList(usort(treeToList t)))