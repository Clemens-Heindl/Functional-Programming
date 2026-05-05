import Data.List

gmean :: [Int] -> Int
gmean a
    | null a = -1
    | any (>1) [length x| x <- (group sorted)] = -1
    | otherwise = last (fst (splitAt midIndex sorted))
    where
        sorted = sort a
        midIndex = div ((length a) + 1) 2

main :: IO ()
main = do
    let array = [1,2,3,4]
        result = gmean array
    putStrLn $ "Mean is " ++ show result