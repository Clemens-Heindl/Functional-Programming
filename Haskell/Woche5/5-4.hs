data Tree = Nil | Node Int Tree Tree

addNodes :: Tree -> Int -> Int -> Int
addNodes Nil _ _ = 0
addNodes (Node root left right) t counter
    | counter > t = 0
    | otherwise = foldl (+) root [addNodes left t (counter+1),addNodes right t (counter+1)]


treesum :: Tree -> Int -> Int
treesum b t
    | t < 0 = 0
    | otherwise = addNodes b t 0
