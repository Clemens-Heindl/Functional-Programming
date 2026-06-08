data BTree = Leaf Char | BNode Int BTree BTree deriving (Show, Eq)
mirror :: BTree -> BTree
mirror (BNode root left right) = (BNode root (mirror right) (mirror left))
mirror c = c