outstanding :: [Int] -> [Int]
outstanding l = [r | r <- l, length (filter (== r) l) == 1]