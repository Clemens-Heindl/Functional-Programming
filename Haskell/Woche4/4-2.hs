data BTree = Leaf Char | BNode Int BTree BTree deriving (Show, Eq)
topmirror :: BTree -> BTree
topmirror (BNode root left right) = (BNode root right left)
topmirror c = c