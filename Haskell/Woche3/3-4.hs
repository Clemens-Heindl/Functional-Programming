reverseString :: String -> String
reverseString [] = []
reverseString (x:xs) = (reverseString xs) ++ [x]

palFree :: String -> Int -> Bool
palFree t@(x:xs) n
    | n < 1 = True
    | n > l = True
    | n == l = (reverseString t) /= t
    | otherwise = palFree xs (n-1) && palFree (init t) (n-1) && palFree t (n+1)
    where 
        l = length t