main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Integer] -> Integer
solve [n, k] | k <= (n + 1) `div` 2 = 2 * k - 1
             | otherwise = 2 * (k - (n + 1) `div` 2)
solve _ = undefined
