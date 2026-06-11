data Tree = Nil | Node Int Tree Tree

cutter :: Tree -> Int -> Int -> Tree
cutter Nil _ _ = Nil
cutter (Node root left right) t count
    | count > t = Nil
    | otherwise = (Node root (cutter left t (count+1)) (cutter right t (count+1)))

cutoff :: Tree -> Int -> Tree
cutoff b t
    | t < 0 = Nil
    |otherwise = cutter b t 0