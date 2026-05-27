import Data.List
toh :: Int -> Char -> Char -> Char -> [(Char, Char)]
toh n a b c
    | n <= 0 = []
    | any (`notElem` "ABC") [a,b,c] = []
    | length [a,b,c] /= length (nub [a,b,c]) = []
    | n == 1 = [(a,b)]
    | otherwise = toh (n-1) a c b ++ [(a,b)] ++ toh (n-1) c b a