main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Integer] -> Integer
solve [n, m, a] = ((n - 1) `quot` a + 1) * ((m - 1) `quot` a + 1)
solve _ = 0
