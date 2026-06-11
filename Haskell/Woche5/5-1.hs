data Tree = Nil | Node Int Tree Tree

treeHeight :: Tree -> Int -> Int
treeHeight Nil count = count
treeHeight (Node root left right) count = max (treeHeight left (count+1)) (treeHeight right (count+1))

traverseTree :: Tree -> Int -> Int -> [Int]
traverseTree (Node root left right) t counter
    | t == counter = [root]
    | otherwise = traverseTree left t (counter+1) ++ traverseTree right t (counter+1)

layer :: Tree -> Int -> [Int]
layer b t
    | t < 0 = []
    | t > treeHeight b (-1) = []
    | otherwise = traverseTree b t 0