main :: IO ()
main = readLn >>= print . solve

solve :: Integer -> Integer
solve n = binomial (2 * n - 2) (n - 1)

binomial :: Integral a => a -> a -> a
binomial n k | k < 0 || n < k = 0
             | otherwise = product [n-k+1..n] `div` product [1..k]
