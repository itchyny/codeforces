main :: IO ()
main = getContents >>= putStrLn . solve . map read . tail . words

solve :: [Int] -> String
solve [] = ""
solve [x] = [1..x] >> "PLR"
solve (x:xs) = ([1..x] >> "PRL") ++ "R" ++ solve xs
