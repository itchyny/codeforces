main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve [a, b, n] = go n 1
  where go m k | m == 0 = k
               | otherwise = go (m - m `gcd` ([b, a] !! k)) (1 - k)
solve _ = undefined
