main :: IO ()
main = getContents >>= print . solve . map read . tail . words

solve :: [Int] -> Int
solve xs = if 1 `elem` xs then -1 else 1
