main :: IO ()
main = getContents >>= print . solve . map read . words

solve :: [Int] -> Int
solve (n:xs) = fst $ head $ filter ((>=n) . snd) $ zip (cycle [1..7]) $ scanl1 (+) (cycle xs)
solve _ = undefined
