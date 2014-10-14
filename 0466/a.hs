main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve [n, m, a, b] = minimum [ n * a, b * (n `div` m) + a * (n `mod` m), b * (n `div` m + 1) ]
solve _ = undefined
