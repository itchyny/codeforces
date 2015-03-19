main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Int] -> Int
solve (_:k:ys) = length (filter (<=5-k) ys) `div` 3
solve _ = undefined
