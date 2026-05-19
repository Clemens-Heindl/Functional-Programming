import Data.List

type Editor = [Char]

replace :: Editor -> Int -> String -> String -> Editor
replace [] count origin new = []
replace str@(head:tail) count origin new 
    | count < 1 = str
    | origin `isPrefixOf` str =
        new ++ replace (drop (length origin) str) (count - 1) origin new
    | otherwise =
        head : replace tail count origin new

main :: IO ()
main = do
    let array = "Hohohoho"
        result = replace array 4 "o" "a"
    putStrLn $ "Result is " ++ show result
