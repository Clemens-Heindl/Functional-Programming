type Relation = [(Int, Int)]

isAntisym :: Relation -> Bool
isAntisym [] = True
isAntisym ((x,y):xs)
     | x /= y && (y,x) `elem` xs = False
     | otherwise = isAntisym xs
    