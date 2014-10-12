main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve [n, k, l, c, d, p, nl, np] = minimum [ k * l `div` nl, c * d, p `div` np ] `div` n
solve _ = undefined
