main :: IO ()
main = getLine >>= putStrLn . solve . map read . words

solve :: [Int] -> String
solve = name . even . minimum

name :: Bool -> String
name True = "Malvika"
name _ = "Akshat"
