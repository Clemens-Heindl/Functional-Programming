data Tree a = Nil | Leaf a | Node a (Tree a) (Tree a) deriving (Show)

instance Eq a => Eq (Tree a) where
    (==) Nil Nil = True
    (==) (Leaf x) (Leaf y) = x == y
    (==) (Node r x y) (Node r2 x2 y2) = r == r2 && x == x2 && y == y2
    (==) _ _ = False

instance Ord a => Ord (Tree a) where
    (<=) Nil Nil = True
    (<=) (Leaf x) (Leaf y) = x <= y
    (<=) (Node r x y) (Node r2 x2 y2) = r <= r2 && x <= x2 && y <= y2
    (<=) _ _ = False
    (<) (Leaf x) (Leaf y) = x < y
    (<) (Node r x y) (Node r2 x2 y2) = r < r2 && x < x2 && y < y2
    (<) _ _ = False
