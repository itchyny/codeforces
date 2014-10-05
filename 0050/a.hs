main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve [m, n] = (m * n) `div` 2
solve _ = 0
