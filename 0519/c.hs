main :: IO ()
main = getContents >>= print . (\[a, b] -> solve a b) . map read . words

solve :: Integer -> Integer -> Integer
solve n m | n + m < 3 || n < 1 || m < 1 = 0
          | n > m = 1 + solve (n - 2) (m - 1)
          | otherwise = 1 + solve (n - 1) (m - 2)
