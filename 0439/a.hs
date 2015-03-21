main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Int] -> Int
solve (n:d:ts) | sum ts + 10 * (n - 1) > d = -1
               | otherwise = (d - sum ts) `div` 5
solve _ = undefined
