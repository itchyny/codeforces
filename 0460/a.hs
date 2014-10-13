main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve [n, m] = go n 1
  where go 0 d = d - 1
        go k d | d `mod` m == 0 = go k (d + 1)
               | otherwise = go (k - 1) (d + 1)
solve _ = undefined
