isPerfect :: Integer -> Bool
isPerfect x = sum [d | d <- [1 .. x-1], x `mod` d == 0] == x

perfect :: Integer -> Integer -> [Integer]
perfect m n
    | m <= 0 = [0]
    | n < m  = [0]
    | otherwise = [x|x <- [m..n], isPerfect x]