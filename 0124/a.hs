main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve [n, a, b] = min (n - a) (b + 1)
solve _ = undefined
