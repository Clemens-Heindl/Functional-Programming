import Data.List
import Data.Char

frequency :: String -> [(Char, Int)]
frequency s = [(head y, length y) | y <- sorted]
    where 
        sorted = group [x | x <- sort s, isLower x]

main :: IO ()
main = do
    let string = "Osffd@f"
        result = frequency string
    putStrLn $ "Frequency is " ++ show result