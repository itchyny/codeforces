main :: IO ()
main = getLine >>= print . solve . map read . words

solve :: [Int] -> Int
solve [n, m] | n < m = -1
             | otherwise = m * ((n `div'` 2) `div'` m)
        where div' a b = - (-a) `div` b
solve _ = undefined
