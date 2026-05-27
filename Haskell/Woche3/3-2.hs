import Data.List

type Graph = [[Int]]


reachableFrom :: Graph -> Int -> [Int]
reachableFrom g m
    | m < 1 || m > length g = []
    | any (\row -> length row /= length g) g = []
    | any (any (`notElem` [0,1])) g = []
    | otherwise = sort(nub (concatMap (reachableWithMemory []) adjacent))
  where
    adjacent =[ x | x <- [1 .. length g], (g !! (m - 1) !! (x - 1)) == 1]
    reachableWithMemory :: [Int] -> Int -> [Int]
    reachableWithMemory memory v
        | v `elem` memory = []
        | otherwise = v: nub(concatMap (reachableWithMemory (v:memory)) (adj v))
    
    adj v = [ x | x <- [1 .. length g], g !! (v - 1) !! (x - 1) == 1]
    
-- copy at own risk this a pretty oddly specific solution (it can surely be done easier)