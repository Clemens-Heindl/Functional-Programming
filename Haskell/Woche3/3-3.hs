import Data.List

type Graph = [[Int]]

isReachable :: Graph -> Int -> Int -> Bool
isReachable g m n
    | any (\x -> x < 1 || x > length g ) [m,n] = False
    | any (\row -> length row /= length g) g = False
    | any (any (`notElem` [0,1])) g = False
    | g !! (m-1) !! (n-1) == 1 = True
    | otherwise = any (reachableWithMemory n []) adjacent
    where 
        adjacent =[ x | x <- [1 .. length g], (g !! (m - 1) !! (x - 1)) == 1]
        reachableWithMemory :: Int -> [Int] -> Int -> Bool
        reachableWithMemory n memory m
            | m `elem` memory = False
            | g !! (m-1) !! (n-1) == 1 = True
            | otherwise = any (reachableWithMemory n (m:memory)) adjacent