main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve [a, b] = go a
  where go n | n < b = n
             | otherwise = b + go (n - b + 1)
solve _ = undefined
