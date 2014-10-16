main :: IO ()
main = getLine >>= putStrLn . unwords . map show . solve . map read . words

solve :: [Integer] -> [Integer]
solve [n, m] = [kmin, kmax]
  where kmax = binomial (n - m + 1) 2
        kmin = (n `mod` m) * binomial (n `div` m + 1) 2 + (m - n `mod` m) * binomial (n `div` m) 2
solve _ = undefined

binomial :: Integral a => a -> a -> a
binomial n k | k < 0 || n < k || n <= 0 = 0
             | k == 0 || n == k = 1
             | otherwise = product [n-k+1..n] `div` product [1..k]
