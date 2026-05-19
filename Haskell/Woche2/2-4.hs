import Data.List

ps :: [Int] -> [[Int]]
ps [] = [[]]
ps l@(x:xs)
    | (length l /= length (nub l)) =  [[],[]]
    | otherwise = [x:s | s <- ps xs] ++ ps xs